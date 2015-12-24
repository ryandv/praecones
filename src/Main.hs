module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import Data.Maybe
import Data.Char

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import System.IO
import System.Process
import System.Random

data Statusbar = Statusbar (TVar (String, String))

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
       modifyTVar' tv (\(lastXMonadStatus, _) -> (lastXMonadStatus, formattedTime))

    threadDelay 1000000 -- one second

xmonadUpdateReader :: Statusbar -> IO ()
xmonadUpdateReader (Statusbar tv) = do
    nextStatus <- getLine

    atomically $ do
        modifyTVar' tv (\(_, lastTime) -> (nextStatus, lastTime))

printCurrentStatusbar :: Handle -> Statusbar -> IO ()
printCurrentStatusbar h (Statusbar tv) = do

    (currentXMonadStatus, currentTime) <- atomically $ do
        currentStatusbarText <- readTVar tv

        return currentStatusbarText

    hPutStrLn h $ "^fg(#e4e4e4)^pa(0)" ++ currentXMonadStatus ++ " ^pa(1800)" ++ currentTime

main :: IO ()
main = do
    -- Print to stdout on demand
    hSetBuffering stdout NoBuffering

    -- Initialize shared state.
    currentStatusbar <- atomically $ do
        tvStatusbar <- newTVar ("", "")
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
