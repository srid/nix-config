{ pkgs, fetchGH, ... }:

let
  ormolu = fetchGH "tweag/ormolu" "39592b2";
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in {
  home.packages = with pkgs.haskellPackages; [
    # ormolu code formatter
    (callPackage ormolu { inherit pkgs; }).ormolu
    # stylish-hashell code formatter
    stylish-haskell
    # Install stable HIE for specified GHC versions
    (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
  ];
}
