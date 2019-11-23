{ pkgs, fetchGH, ... }:

let
  ormolu = fetchGH "tweag/ormolu" "39592b2";
in {
  home.packages = with pkgs.haskellPackages; [
    (callPackage ormolu { inherit pkgs; }).ormolu
    stylish-haskell
  ];
}
