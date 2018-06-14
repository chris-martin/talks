{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables, TypeApplications #-}

module MonadicParty.Either (main) where

-- base
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

-- text
import qualified Data.Text.Lazy

main :: IO ()
main = do

    IO.hSetBuffering IO.stdout IO.LineBuffering

    socketsMaybe :: Maybe [Socket] <- getActivatedSockets

    case socketsMaybe of

        Nothing ->
            Scotty.scotty 8001 scottyApp

        Just [socket] ->
          do
            Socket.setNonBlockIfNeeded (Socket.fdSocket socket)
            Scotty.scottySocket (def @Scotty.Options) socket scottyApp

        Just sockets -> die ("Wrong number of sockets: " ++
                             show (length sockets))

scottyApp :: Scotty.ScottyM ()
scottyApp =
    Scotty.get "/either" $ do
        Scotty.html (Data.Text.Lazy.fromStrict [text|
            <!doctype html>
            <html>
                <head></head>
                <body>
                    This one can serve either a port number or an activated socket.
                </body>
            </html>
          |])
