{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
, doCheck ? true
}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
    };
  };
  toggleCheck = if doCheck then pkgs.haskell.lib.doCheck else pkgs.haskell.lib.dontCheck;

  drv =
    toggleCheck
    (pkgs.haskell.lib.dontHaddock
     (haskellPackages.callCabal2nix "pandoc-include-code" ./. {}));
in
drv
