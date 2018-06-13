let

    # Using unstable to get cabal 2.2 in which new-run works
    pkgs = import <unstable> {};

    ghc = pkgs.haskell.packages.ghc822;

    hs = ghc.developPackage {
        root = ./run-slides;
        source-overrides = {
            fsnotify = "0.2.1.1";
            hinotify = "0.3.9";
        };
    };

    cabal = ghc.cabal-install;

    run-slides = pkgs.writeScriptBin "run-slides"
        ''
            cabal --version
            cabal new-run exe:run-slides
        '';

in
    hs.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [ cabal run-slides pkgs.pandoc ];
    })
