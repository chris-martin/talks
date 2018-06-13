
# The shell file is for developers. It defines an environment
# that we use to do dev tasks like running a REPL.

let
    pkgs = import <nixpkgs> { config.allowUnfree = true; };

    monadic-party = import ./default.nix { inherit pkgs; };

    ghc = pkgs.haskell.packages.${import ./ghc-version.nix};

    cabal = ghc.cabal-install;

    party-ghci = pkgs.writeScriptBin "party-ghci"
        ''
            ${cabal}/bin/cabal new-repl "$@"
        '';

    party-ghcid = pkgs.writeScriptBin "party-ghcid"
        ''
            ${ghc.ghcid}/bin/ghcid --command "${cabal}/bin/cabal new-repl" "$@"
        '';

    devTools = [
        party-ghci
        party-ghcid
    ];

in
    monadic-party.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ devTools;
    })
