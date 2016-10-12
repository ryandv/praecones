module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM

import Control.Exception

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.IORef

import Data.Maybe

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import System.IO
import System.Process

data Statusbar = Statusbar
  { xmonadSection :: String
  , datetimeSection :: String
  } deriving(Show)

data EventQueue a = EventQueue (TVar Int) (TVar [a])

newEventQueue :: Int -> IO (EventQueue a)
newEventQueue size = atomically $ do
  sizeVar <- newTVar size
  events <- newTVar []
  return $ EventQueue sizeVar events

instance Eq Statusbar where
  (==) x y = (xmonadSection x == xmonadSection y) && (datetimeSection x == datetimeSection y)

{-- CONFIG --}

dzen2Spec :: CreateProcess
dzen2Spec = CreateProcess
    (ShellCommand "dzen2 -fn '-*-terminus-*-*-*-*-*-320-*-*-*-*-*-*'")
    Nothing
    Nothing
    CreatePipe
    CreatePipe
    CreatePipe
    False
    False
    False

timeFormat :: String
timeFormat = "%Y %m %d %X"

maxLen :: Int
maxLen = 2

logMessage :: String -> IO ()
logMessage msg = (flip catch) ((\e -> return ()) :: SomeException -> IO ()) $ do
  withFile "/tmp/praecones.log" AppendMode $ \h -> do
    hSetBuffering h NoBuffering
    hPutStrLn h msg

logException :: SomeException -> IO ()
logException ex = logMessage (show ex)

{-- END CONFIG --}

main :: IO ()
main = do
    (flip catch) logException $ do
      -- Print to stdout on demand
      hSetBuffering stdout NoBuffering

      -- Initialize shared state.
      xmonadEventQueue <- newEventQueue maxLen
      systemTimeEventQueue <- newEventQueue maxLen

      -- Initialize thread-local mutable statusbar ref.
      statusbar <- newIORef $ Statusbar "" ""

      -- Initialize dzen2 and hook up to its STDIN
      (dzen2Stdin, dzen2Stdout, dzen2Stderr, dzen2Ph) <- createProcess dzen2Spec

      -- Assume we can acquire a handle for now...
      let dzen2Stdin' = fromJust $ dzen2Stdin
      hSetBuffering dzen2Stdin' NoBuffering

      -- Create tick threads.
      tickTimeThread <- forkIO . forever $ systemTimeTicker systemTimeEventQueue
      readXmonadUpdatesThread <- forkIO . forever $ xmonadUpdateReader xmonadEventQueue

      -- Print to dzen2.
      forever $ printCurrentStatusbar dzen2Stdin' statusbar xmonadEventQueue systemTimeEventQueue

printCurrentStatusbar :: Handle -> IORef Statusbar -> EventQueue String -> EventQueue String -> IO ()
printCurrentStatusbar h ioref xmonadEvents timeEvents = do
    currentStatusbar <- readIORef ioref

    nextStatusbar <- atomically $
        updateTimeSection currentStatusbar timeEvents `orElse`
        updateXmonadSection currentStatusbar xmonadEvents

    writeIORef ioref nextStatusbar

    liftIO . hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection nextStatusbar) ++ " ^pa(1500)" ++ (datetimeSection nextStatusbar)

updateTimeSection :: Statusbar -> EventQueue String -> STM Statusbar
updateTimeSection statusbar events = do
    newSection <- consumer events
    return $ Statusbar (xmonadSection statusbar) newSection

systemTimeTicker :: EventQueue String -> IO ()
systemTimeTicker (EventQueue size events) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    atomically $ producer (EventQueue size events) formattedTime

updateXmonadSection :: Statusbar -> EventQueue String -> STM Statusbar
updateXmonadSection statusbar events = do
    newSection <- consumer events
    return $ Statusbar newSection (datetimeSection statusbar)

xmonadUpdateReader :: EventQueue String -> IO ()
xmonadUpdateReader (EventQueue size events) = do
    nextStatus <- getLine

    atomically $ producer (EventQueue size events) nextStatus

consumer :: EventQueue String -> STM String
consumer (EventQueue size events) = do
    avail <- readTVar size

    writeTVar size (avail + 1)

    evs <- readTVar events
    case evs of
      [] -> retry
      (e:es) -> do
        writeTVar events es
        return e

producer :: EventQueue String -> String -> STM ()
producer (EventQueue size events) statusbarUpdate = do
    avail <- readTVar size
    when (avail == 0) retry

    evs <- readTVar events

    writeTVar size (avail - 1)
    writeTVar events $ evs ++ [statusbarUpdate]
