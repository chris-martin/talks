# nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz

(import <nixpkgs> { }).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";

    # NixOS 18.03, 2018 Mar 18
    rev = "0e7c9b32817e5cbe61212d47a6cf9bcd71789322";
    sha256 = "1dm777cmlhqcwlrq8zl9q2d87h3p70rclpvq36y43kp378f3pd0y";
}
