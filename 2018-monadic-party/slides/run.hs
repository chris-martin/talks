{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Turtle

import qualified Prelude
import qualified System.IO as IO

import Control.Concurrent.Async (async)
import Data.List (elem, intercalate)
import Network.Wai.Middleware.Static (static)
import System.FilePath (takeExtension)
import System.FSNotify (watchTree, withManager, eventPath)
import Web.Scotty (scotty, middleware)

main :: IO ()
main = do

    async (build *> watch)

    scotty 3000 (middleware static)

-- Start a watching job (in the background; this action returns immediately).
watch :: IO ()
watch = do

    putStrLn ("Watching these extensions: " ++
              intercalate ", " watchedExts)

    withManager $ \mgr ->
        watchTree mgr "." (isWatched . eventPath) (const build)

isWatched :: Prelude.FilePath -> Bool
isWatched path =

    takeExtension path `elem` watchedExts

watchedExts :: [String]
watchedExts =

    [".md", ".png", ".jpg", ".nix", ".css"]

build :: IO ()
build = do

    putStr "Building slides..."
    IO.hFlush IO.stdout
    sh (procs "nix-shell" ["-p", "pandoc", "--run", buildCommand] empty)
    putStrLn " Done."

buildCommand :: Text
buildCommand =

    "pandoc -s -t slidy --slide-level=2 -V slidy-url=./slidy slides.md -o slides.html"
