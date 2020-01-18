{
  nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
, sources ? import ./nix/sources.nix
} :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskellPackages;
in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (./.) {}

