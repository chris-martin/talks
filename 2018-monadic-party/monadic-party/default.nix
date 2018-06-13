{ pkgs }:

let

    ghc = pkgs.haskell.packages.${import ./ghc-version.nix};

    developPackage =
      { root
      , source-overrides ? {}
      , overrides ? self: super: {}
      }:
      let
          extensions = pkgs.lib.composeExtensions
              (ghc.packageSourceOverrides source-overrides)
              overrides;

          ghc' = ghc.extend extensions;

          package = ghc'.callCabal2nix (builtins.baseNameOf root) root {};

      in
          pkgs.haskell.lib.overrideCabal package (drv: {
              enableSharedExecutables = false;
              isLibrary = false;
              doHaddock = false;
              postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
          });

in

developPackage {

  root = ./.;

  source-overrides = {

    # Any specific Haskell package versions you need to pin go here, e.g.:
    # scotty = "0.11.1";

  };

}
