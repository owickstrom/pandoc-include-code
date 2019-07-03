{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
}:
nixpkgs.pkgs.haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [(import ./. {inherit compiler nixpkgs;})];
  buildInputs = with nixpkgs.pkgs; [
    pandoc
  ];
}
