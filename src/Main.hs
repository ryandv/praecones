module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

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

{-- END CONFIG --}

applyStatusbarUpdate :: Statusbar -> StatusbarUpdateEvent -> Statusbar
applyStatusbarUpdate si ev = Statusbar nextXmonadSection nextDatetimeSection
  where
    nextXmonadSection = fromMaybe (xmonadSection si) (newXmonadSection ev)
    nextDatetimeSection = fromMaybe (datetimeSection si) (newDatetimeSection ev)

systemTimeTicker :: EventQueue StatusbarUpdateEvent -> IO ()
systemTimeTicker (EventQueue chan) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    let statusbarUpdate = StatusbarUpdateEvent Nothing (Just formattedTime)
    writeChan chan statusbarUpdate

    threadDelay 1000000 -- one second

xmonadUpdateReader :: EventQueue StatusbarUpdateEvent -> IO ()
xmonadUpdateReader (EventQueue chan) = do
    nextStatus <- getLine

    writeChan chan $ StatusbarUpdateEvent (Just nextStatus) Nothing

printCurrentStatusbar :: Handle -> IORef Statusbar -> EventQueue StatusbarUpdateEvent -> IO ()
printCurrentStatusbar h ioref (EventQueue chan) = do
    currentStatusbar <- readIORef ioref

    nextStatusbar <- do
        nextEvent <- readChan chan
        return $ applyStatusbarUpdate currentStatusbar nextEvent

    writeIORef ioref nextStatusbar

    liftIO . hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection nextStatusbar) ++ " ^pa(1500)" ++ (datetimeSection nextStatusbar)

main :: IO ()
main = do
    -- Print to stdout on demand
    hSetBuffering stdout NoBuffering

    -- Initialize shared state.
    eventQueue <- fmap EventQueue $ newChan

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
