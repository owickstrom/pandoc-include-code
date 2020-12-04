{ pkgs ? import <nixpkgs> { }, compiler ? "ghc884" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: { };
  };
in haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [ (import ./. { inherit compiler pkgs; }) ];
  buildInputs = with pkgs; [ cabal-install pandoc ];
}
