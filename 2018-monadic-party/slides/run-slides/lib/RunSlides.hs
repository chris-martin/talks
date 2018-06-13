{-# LANGUAGE OverloadedStrings #-}

module RunSlides (main) where

import Turtle

import qualified System.IO as IO

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception.Safe (SomeException, withException)
import Control.Monad (forever)
import Data.List (elem, intercalate)
import Network.Wai.Middleware.Static (static)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeExtension)
import System.FSNotify (watchDir, watchTree, withManager, eventPath)
import Web.Scotty (scotty, middleware)

main :: IO ()
main = do

    IO.hSetBuffering IO.stdout IO.LineBuffering

    build

    putStrLn ("Watching these extensions: " ++
              intercalate ", " watchedExts)

    _ <- withManager $ \mgr -> do
        watchDir mgr "." (isWatched . eventPath) (const build)
        scotty 3000 (middleware static)

    pure ()

isWatched :: Prelude.FilePath -> Bool
isWatched path =

    takeExtension path `elem` watchedExts

watchedExts :: [String]
watchedExts =

    [".md", ".png", ".jpg", ".nix", ".css"]

build :: IO ()
build = do

    putStr "Building slides..." *> IO.hFlush IO.stdout
    procs "pandoc"
        [ "-s", "-t", "slidy", "--slide-level=2"
        , "-V", "slidy-url=./slidy"
        , "slides.md", "-o", "slides.html"
        ] empty
    putStrLn " Done."
