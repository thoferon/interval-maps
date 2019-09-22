{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = pkgs.haskell.packages.ghc865;

  interval-map = hsPkgs.callCabal2nix "interval-maps" ./. {};

in
hsPkgs.shellFor {
  packages = p: [interval-map];
  buildInputs = with pkgs; [ cabal-install ];
}
