{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "slides-html"
    {
        buildInputs = [ pkgs.pandoc ];
    }
    ''
        mkdir -p $out/slides
        pandoc -s -t slidy --slide-level=2 -V slidy-url=./slidy ${./slides.md} -o $out/slides/index.html
        ln -s ${./slidy} $out/slides/slidy
        ln -s ${./icons} $out/slides/icons
        ln -s ${./diagrams} $out/slides/diagrams
        ln -s ${./screenshots} $out/slides/screenshots
    ''
