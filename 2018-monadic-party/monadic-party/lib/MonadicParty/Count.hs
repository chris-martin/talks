{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables, TypeApplications #-}

module MonadicParty.Count (main) where

-- async
import Control.Concurrent.Async (Concurrently (..))

-- base
import Control.Monad (forever)
import Numeric.Natural
import System.Exit (die)
import qualified System.IO as IO

-- data-default-class
import Data.Default.Class (def)

-- neat-interpolation
import NeatInterpolation

-- network
import Network.Socket (Socket)
import qualified Network.Socket as Socket

-- scotty
import qualified Web.Scotty as Scotty

-- socket-activation
import Network.Socket.Activation (getActivatedSockets)

-- stm
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

-- text
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.IO

main :: IO ()
main = do

    IO.hSetBuffering IO.stdout IO.LineBuffering

    socketsMaybe :: Maybe [Socket] <- getActivatedSockets

    socket <- case socketsMaybe of

        Just [s] ->
          do
            Socket.setNonBlockIfNeeded (Socket.fdSocket s)
            pure s

        Just sockets -> die ("Wrong number of sockets: " ++
                             show (length sockets))

        Nothing -> die "Not socket activated"

    logHandle <- newLog

    countVar <- newTVarIO 0

    let
      runServer = Scotty.scottySocket
        (def @Scotty.Options) socket (scottyApp logHandle countVar)

    runConcurrently $
        Concurrently (runLogger logHandle) *>
        Concurrently runServer

-----------------  Logging  --------------------

data LogHandle = LogHandle (TChan Text)

newLog :: IO LogHandle
newLog =
    LogHandle <$> newTChanIO

writeToLog :: LogHandle -> Text -> IO ()
writeToLog (LogHandle chan) message =
    atomically (writeTChan chan message)

runLogger :: LogHandle -> IO a
runLogger (LogHandle chan) =
    forever $ do
        message <- atomically (readTChan chan)
        Data.Text.IO.putStrLn message

------------------------------------------------


scottyApp :: LogHandle -> TVar Natural -> Scotty.ScottyM ()
scottyApp logHandle countVar =
    Scotty.get "/count" $ do

        countText <- Scotty.liftAndCatchIO . atomically $ do
            previousCount <- readTVar countVar
            let newCount = previousCount + 1
            writeTVar countVar newCount
            (pure . Data.Text.pack . show @Natural) newCount

        Scotty.liftAndCatchIO (writeToLog logHandle countText)

        Scotty.html (Data.Text.Lazy.fromStrict [text|
            <!doctype html>
            <html>
                <head></head>
                <body>
                    The count: $countText
                </body>
            </html>
          |])
