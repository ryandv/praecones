module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import System.IO
import System.Process

data Statusbar = Statusbar (TVar StatusbarInfo)

data StatusbarInfo = StatusbarInfo
  { xmonadSection :: String
  , datetimeSection :: String
  , version :: Int
  }

instance Eq StatusbarInfo where
  (==) x y = (xmonadSection x == xmonadSection y) && (datetimeSection x == datetimeSection y)

{-- CONFIG --}

dzen2Spec :: CreateProcess
dzen2Spec = CreateProcess
    (ShellCommand "dzen2")
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

systemTimeTicker :: Statusbar -> IO ()
systemTimeTicker (Statusbar tv) = do
    curTime <- getCurrentTime
    localZonedTime <- utcToLocalZonedTime curTime

    let localCurTime = zonedTimeToLocalTime localZonedTime
    let formattedTime = formatTime defaultTimeLocale timeFormat localCurTime

    atomically $ do
       modifyTVar' tv $ updateStatusbarInfo formattedTime

    threadDelay 1000000 -- one second

  where
    updateStatusbarInfo :: String -> StatusbarInfo -> StatusbarInfo
    updateStatusbarInfo newFormattedTime si = StatusbarInfo (xmonadSection si) newFormattedTime (version si)

xmonadUpdateReader :: Statusbar -> IO ()
xmonadUpdateReader (Statusbar tv) = do
    nextStatus <- getLine

    atomically $ do
        modifyTVar' tv $ updateStatusbarInfo nextStatus

  where
    updateStatusbarInfo :: String -> StatusbarInfo -> StatusbarInfo
    updateStatusbarInfo newXmonadSection si = StatusbarInfo newXmonadSection (datetimeSection si) (version si)

printCurrentStatusbar :: Handle -> Statusbar -> IO ()
printCurrentStatusbar h (Statusbar tv) = do

    currentStatusbarInfo<- atomically $ do
        currentStatusbarText <- readTVar tv

        return currentStatusbarText

    hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ (xmonadSection currentStatusbarInfo) ++ " ^pa(1800)" ++ (datetimeSection currentStatusbarInfo)

main :: IO ()
main = do
    -- Print to stdout on demand
    hSetBuffering stdout NoBuffering

    -- Initialize shared state.
    currentStatusbar <- atomically $ do
        tvStatusbar <- newTVar $ StatusbarInfo "" "" 0
        return $ Statusbar tvStatusbar

    -- Initialize dzen2 and hook up to its STDIN
    (dzen2Stdin, dzen2Stdout, dzen2Stderr, dzen2Ph) <- createProcess dzen2Spec

    -- Assume we can acquire a handle for now...
    let dzen2Stdin' = fromJust $ dzen2Stdin

    -- Create tick threads.
    tickTimeThread <- forkIO . forever $ systemTimeTicker currentStatusbar
    readXmonadUpdatesThread <- forkIO . forever $ xmonadUpdateReader currentStatusbar

    -- Print to dzen2.
    forever $ printCurrentStatusbar dzen2Stdin' currentStatusbar
