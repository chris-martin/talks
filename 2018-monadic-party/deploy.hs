#! /usr/bin/env stack
-- stack script --resolver lts-11.8 --no-nix-pure

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import NeatInterpolation (text)
import Turtle hiding (text)
import qualified Data.Text

main :: IO ()
main =
  sh $ do
    nixpkgs <- getNixpkgs
    setNixPath nixpkgs

    server <- getServer   -- Read the server address from a file
    path <- build         -- (1) Build NixOS for our server
    upload server path    -- (2) Upload the build to the server
    activate server path  -- (3) Start running the new version

newtype Nixpkgs = Nixpkgs Text

-- The path of the NixOS build that we're deploying, e.g.
-- "/nix/store/bbcbh0vv64v5bgdhdd4a8fw3p438iiam-nixos-system-unnamed-18.03.132618.0f73fef53a9"
newtype NixOS = NixOS Text

-- The address of the server to which we're deploying, e.g.
-- Server "54.175.33.139" "22"
data Server = Server Text Text

getNixpkgs :: Shell Nixpkgs
getNixpkgs =
  do
    line <- single (inproc command args empty)
    return (Nixpkgs (lineToText line))

  where
    command = "nix-build"
    args = ["nixpkgs.nix", "--no-out-link"]

setNixPath :: Nixpkgs -> Shell ()
setNixPath (Nixpkgs path) =
    export "NIX_PATH" ("nixpkgs=" <> path)

-- Read the server address from a file.
getServer :: Shell Server
getServer =
  do
    line <- single (input "server-address.txt")

    let
      (host, port) = case Data.Text.splitOn ":" (lineToText line) of
        [x, y] -> (x, y)
        _ -> fail "Failed to parse server address"

    return (Server host port)

-- Build NixOS for our server.
build :: Shell NixOS
build =
  do
    echo "Building NixOS..."
    line <- single (inproc command args empty)
    return (NixOS (lineToText line))

  where
    command = "nix-build"
    args = ["server.nix", "--no-out-link"]

-- Upload the build to the server.
upload :: Server -> NixOS -> Shell ()
upload (Server host port) (NixOS path) =

  do
    echo "Uploading NixOS..."
    export "NIX_SSHOPTS" ("-p " <> port)
    procs command args empty

  where
    command = "nix-copy-closure"
    args = ["--use-substitutes", host, path]

-- Start running the new version of NixOS on the server.
activate :: Server -> NixOS -> Shell ()
activate (Server host port) (NixOS path) =

  do
    echo "Activating NixOS..."
    procs command args empty

  where
    command = "ssh"
    args = ["-p", port, host, remoteCommand]
    profile = "/nix/var/nix/profiles/system"
    remoteCommand = [text|
        sudo nix-env --profile $profile --set $path
        sudo $profile/bin/switch-to-configuration switch
      |]
