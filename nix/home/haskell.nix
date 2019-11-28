{ pkgs, fetchGH, ... }:

let
  ormolu = fetchGH "tweag/ormolu" "6e07126";
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in {
  home.packages = with pkgs.haskellPackages; [
    # Some commonly used tools
    cachix
    pandoc

    # ormolu code formatter
    (callPackage ormolu { inherit pkgs; }).ormolu
    # stylish-hashell code formatter
    stylish-haskell
    # Install stable HIE for specified GHC versions
    (all-hies.selection { selector = p: { inherit (p) ghc865 ghc864; }; })
  ];

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
