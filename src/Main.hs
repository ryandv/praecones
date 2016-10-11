module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

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

data EventQueue a = EventQueue (Chan a)

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
maxLen = 1

logMessage :: String -> IO ()
logMessage msg = do
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
      producerWakeup <- newEmptyMVar
      consumerWakeup <- newEmptyMVar
      eventQueueLen <- newIORef 0
      eventQueue <- fmap EventQueue $ newChan

      -- Initialize thread-local mutable statusbar ref.
      statusbar <- newIORef $ Statusbar "" ""

      -- Initialize dzen2 and hook up to its STDIN
      (dzen2Stdin, dzen2Stdout, dzen2Stderr, dzen2Ph) <- createProcess dzen2Spec

      -- Assume we can acquire a handle for now...
      let dzen2Stdin' = fromJust $ dzen2Stdin
      hSetBuffering dzen2Stdin' NoBuffering

      -- Create tick threads.
      tickTimeThread <- forkIO . forever $ systemTimeTicker producerWakeup consumerWakeup eventQueueLen eventQueue
      readXmonadUpdatesThread <- forkIO . forever $ xmonadUpdateReader producerWakeup consumerWakeup eventQueueLen eventQueue

      -- Print to dzen2.
      forever $ printCurrentStatusbar producerWakeup consumerWakeup eventQueueLen dzen2Stdin' statusbar eventQueue

printCurrentStatusbar :: MVar () -> MVar () -> IORef Int -> Handle -> IORef Statusbar -> EventQueue StatusbarUpdateEvent -> IO ()
printCurrentStatusbar producerWakeup consumerWakeup lenref h ioref (EventQueue chan) = do
    currentStatusbar <- readIORef ioref

    nextEvent <- consumer producerWakeup consumerWakeup lenref (EventQueue chan)
    let nextStatusbar = applyStatusbarUpdate currentStatusbar nextEvent

    writeIORef ioref nextStatusbar

    liftIO . hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection nextStatusbar) ++ " ^pa(1500)" ++ (datetimeSection nextStatusbar)

systemTimeTicker :: MVar () -> MVar () -> IORef Int -> EventQueue StatusbarUpdateEvent -> IO ()
systemTimeTicker producerWakeup consumerWakeup lenref (EventQueue chan) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    let statusbarUpdate = StatusbarUpdateEvent Nothing (Just formattedTime)

    producer "TIME" producerWakeup consumerWakeup lenref (EventQueue chan) statusbarUpdate

xmonadUpdateReader :: MVar () -> MVar () -> IORef Int -> EventQueue StatusbarUpdateEvent -> IO ()
xmonadUpdateReader producerWakeup consumerWakeup lenref (EventQueue chan) = do
    nextStatus <- getLine
    logMessage "xmonad update received"

    let statusbarUpdate = StatusbarUpdateEvent (Just nextStatus) Nothing

    producer "XMONAD" producerWakeup consumerWakeup lenref (EventQueue chan) statusbarUpdate

consumer :: MVar () -> MVar () -> IORef Int -> EventQueue StatusbarUpdateEvent -> IO StatusbarUpdateEvent
consumer producerWakeup consumerWakeup lenref (EventQueue chan) = do
    len <- readIORef lenref

    when (len == 0) $ do
      logMessage "consumer waiting"
      takeMVar consumerWakeup

    newLen <- readIORef lenref;                                     logMessage $ "consumer proceeding with length " ++ (show newLen)
    nextEvent <- readChan chan;                                     logMessage "consumer reading from chan"

    writeIORef lenref (newLen - 1)                               >> logMessage "consumer decreasing count"
    when (newLen == maxLen) $ putMVar producerWakeup ()          >> logMessage "waking up producer"
    return nextEvent

producer :: String -> MVar () -> MVar () -> IORef Int -> EventQueue StatusbarUpdateEvent -> StatusbarUpdateEvent -> IO ()
producer name producerWakeup consumerWakeup lenref (EventQueue chan) statusbarUpdate = do
    len <- readIORef lenref

    when (len == maxLen) $ do
      logMessage $ name ++ " waiting"
      takeMVar producerWakeup

    newLen <- readIORef lenref;                                     logMessage $ name ++ " proceeding with length " ++ (show newLen)

    writeChan chan statusbarUpdate                               >> (logMessage $ name ++ " writing to chan")
    writeIORef lenref (newLen + 1)                               >> (logMessage $ name ++ " increasing count")

    when (newLen == 0) $  putMVar consumerWakeup ()              >> logMessage "waking up consumer"
