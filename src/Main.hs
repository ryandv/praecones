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

data StatusbarUpdateEvent = StatusbarUpdateEvent
  { newXmonadSection :: Maybe String
  , newDatetimeSection :: Maybe String
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

applyStatusbarUpdate :: Statusbar -> StatusbarUpdateEvent -> Statusbar
applyStatusbarUpdate si ev = Statusbar nextXmonadSection nextDatetimeSection
  where
    nextXmonadSection = fromMaybe (xmonadSection si) (newXmonadSection ev)
    nextDatetimeSection = fromMaybe (datetimeSection si) (newDatetimeSection ev)

{-- END CONFIG --}

main :: IO ()
main = do
    (flip catch) logException $ do
      -- Print to stdout on demand
      hSetBuffering stdout NoBuffering

      -- Initialize shared state.
      eventQueue <- newEventQueue maxLen

      -- Initialize thread-local mutable statusbar ref.
      statusbar <- newIORef $ Statusbar "" ""

      -- Initialize dzen2 and hook up to its STDIN
      (dzen2Stdin, dzen2Stdout, dzen2Stderr, dzen2Ph) <- createProcess dzen2Spec

      -- Assume we can acquire a handle for now...
      let dzen2Stdin' = fromJust $ dzen2Stdin
      hSetBuffering dzen2Stdin' NoBuffering

      -- Create tick threads.
      tickTimeThread <- forkIO . forever $ systemTimeTicker eventQueue
      readXmonadUpdatesThread <- forkIO . forever $ xmonadUpdateReader eventQueue

      -- Print to dzen2.
      forever $ printCurrentStatusbar dzen2Stdin' statusbar eventQueue

printCurrentStatusbar :: Handle -> IORef Statusbar -> EventQueue StatusbarUpdateEvent -> IO ()
printCurrentStatusbar h ioref (EventQueue size events) = do
    currentStatusbar <- readIORef ioref

    nextEvent <- consumer (EventQueue size events)

    let nextStatusbar = applyStatusbarUpdate currentStatusbar nextEvent
    writeIORef ioref nextStatusbar

    liftIO . hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection nextStatusbar) ++ " ^pa(1500)" ++ (datetimeSection nextStatusbar)

systemTimeTicker :: EventQueue StatusbarUpdateEvent -> IO ()
systemTimeTicker (EventQueue size events) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    let statusbarUpdate = StatusbarUpdateEvent Nothing (Just formattedTime)

    producer (EventQueue size events) statusbarUpdate

xmonadUpdateReader :: EventQueue StatusbarUpdateEvent -> IO ()
xmonadUpdateReader (EventQueue size events) = do
    nextStatus <- getLine

    let statusbarUpdate = StatusbarUpdateEvent (Just nextStatus) Nothing

    producer (EventQueue size events) statusbarUpdate

consumer :: EventQueue StatusbarUpdateEvent -> IO StatusbarUpdateEvent
consumer (EventQueue size events) = atomically $ do
    avail <- readTVar size

    writeTVar size (avail + 1)

    evs <- readTVar events
    case evs of
      [] -> retry
      (e:es) -> do
        writeTVar events es
        return e

producer :: EventQueue StatusbarUpdateEvent -> StatusbarUpdateEvent -> IO ()
producer (EventQueue size events) statusbarUpdate = atomically $ do
    avail <- readTVar size
    when (avail == 0) retry

    evs <- readTVar events

    writeTVar size (avail - 1)
    writeTVar events $ evs ++ [statusbarUpdate]
