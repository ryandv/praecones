module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.QSem

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
      producerSemaphore <- newQSem maxLen
      consumerSemaphore <- newQSem 0
      eventQueue <- fmap EventQueue $ newChan

      -- Initialize thread-local mutable statusbar ref.
      statusbar <- newIORef $ Statusbar "" ""

      -- Initialize dzen2 and hook up to its STDIN
      (dzen2Stdin, dzen2Stdout, dzen2Stderr, dzen2Ph) <- createProcess dzen2Spec

      -- Assume we can acquire a handle for now...
      let dzen2Stdin' = fromJust $ dzen2Stdin
      hSetBuffering dzen2Stdin' NoBuffering

      -- Create tick threads.
      tickTimeThread <- forkIO . forever $ systemTimeTicker producerSemaphore consumerSemaphore eventQueue
      readXmonadUpdatesThread <- forkIO . forever $ xmonadUpdateReader producerSemaphore consumerSemaphore eventQueue

      -- Print to dzen2.
      forever $ printCurrentStatusbar producerSemaphore consumerSemaphore dzen2Stdin' statusbar eventQueue

printCurrentStatusbar :: QSem -> QSem -> Handle -> IORef Statusbar -> EventQueue StatusbarUpdateEvent -> IO ()
printCurrentStatusbar producerSemaphore consumerSemaphore h ioref (EventQueue chan) = do
    currentStatusbar <- readIORef ioref

    nextEvent <- consumer producerSemaphore consumerSemaphore (EventQueue chan)
    let nextStatusbar = applyStatusbarUpdate currentStatusbar nextEvent

    writeIORef ioref nextStatusbar

    liftIO . hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection nextStatusbar) ++ " ^pa(1500)" ++ (datetimeSection nextStatusbar)

systemTimeTicker :: QSem -> QSem -> EventQueue StatusbarUpdateEvent -> IO ()
systemTimeTicker producerSemaphore consumerSemaphore (EventQueue chan) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    let statusbarUpdate = StatusbarUpdateEvent Nothing (Just formattedTime)

    producer "TIME" producerSemaphore consumerSemaphore (EventQueue chan) statusbarUpdate

xmonadUpdateReader :: QSem -> QSem -> EventQueue StatusbarUpdateEvent -> IO ()
xmonadUpdateReader producerSemaphore consumerSemaphore (EventQueue chan) = do
    nextStatus <- getLine
    logMessage "xmonad update received"

    let statusbarUpdate = StatusbarUpdateEvent (Just nextStatus) Nothing

    producer "XMONAD" producerSemaphore consumerSemaphore (EventQueue chan) statusbarUpdate

consumer :: QSem -> QSem -> EventQueue StatusbarUpdateEvent -> IO StatusbarUpdateEvent
consumer producerSemaphore consumerSemaphore (EventQueue chan) = do
    logMessage "consumer waiting on semaphore"
    waitQSem consumerSemaphore

    logMessage "consumer proceeding"

    nextEvent <- readChan chan;                                        logMessage "consumer reading from chan"
    signalQSem producerSemaphore                                    >> logMessage "consumer increasing producer semaphore"
    return nextEvent

producer :: String -> QSem -> QSem -> EventQueue StatusbarUpdateEvent -> StatusbarUpdateEvent -> IO ()
producer name producerSemaphore consumerSemaphore (EventQueue chan) statusbarUpdate = do
    logMessage $ name ++ " waiting on semaphore"
    waitQSem producerSemaphore

    logMessage $ name ++ " proceeding"

    writeChan chan statusbarUpdate                               >> (logMessage $ name ++ " writing to chan")
    signalQSem consumerSemaphore                                 >> (logMessage $ name ++ " increasing consumer semaphore")
