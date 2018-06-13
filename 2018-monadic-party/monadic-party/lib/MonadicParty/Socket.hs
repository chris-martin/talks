{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables, TypeApplications #-}

module MonadicParty.Socket (main) where

-- base
import System.Exit (die)

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

    socketsMaybe :: Maybe [Socket] <- getActivatedSockets

    case socketsMaybe of

        Just [socket] ->
          do
            Socket.setNonBlockIfNeeded (Socket.fdSocket socket)
            Scotty.scottySocket (def @Scotty.Options) socket scottyApp

        Just sockets -> die ("Wrong number of sockets: " ++
                             show (length sockets))

        Nothing -> die "Not socket activated"

scottyApp :: Scotty.ScottyM ()
scottyApp =
    Scotty.get "/socket" $
        Scotty.html (Data.Text.Lazy.fromStrict [text|
            <!doctype html>
            <html>
                <head></head>
                <body>
                    This one is socket activated.
                </body>
            </html>
          |])
