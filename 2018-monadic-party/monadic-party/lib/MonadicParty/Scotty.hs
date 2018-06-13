{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module MonadicParty.Scotty (main) where

-- base
import qualified System.IO as IO

-- neat-interpolation
import NeatInterpolation

-- scotty
import qualified Web.Scotty as Scotty

-- text
import qualified Data.Text.Lazy

main :: IO ()
main =
    Scotty.scotty 8000 scottyApp

scottyApp :: Scotty.ScottyM ()
scottyApp =
    Scotty.get "/scotty" $ do
        Scotty.html (Data.Text.Lazy.fromStrict [text|
            <!doctype html>
            <html>
                <head></head>
                <body>
                    Hello, I am a very basic Scotty demo,
                    serving (internally) on port 8000.
                </body>
            </html>
          |])
