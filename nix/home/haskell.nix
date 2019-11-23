{ pkgs, fetchGH, ... }:

let
  ormolu = import (fetchGH "tweag/ormolu" "39592b2") { inherit pkgs; };
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      "${ormolu.ormoluCompiler}" = pkgs.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
in {
  home.packages = with pkgs.haskellPackages; [
    haskell.packages.${ormolu.ormoluCompiler}.ormolu
    stylish-haskell
  ];
}
