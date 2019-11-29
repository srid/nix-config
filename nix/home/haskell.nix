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
    # ghci
    ".ghci".text = ''
      :set prompt "λ> "
    '';
    # stylish-haskell
    ".stylish-haskell.yaml".text = ''
      steps:
        - imports:
            align: none
            list_align: after_alias
            long_list_align: inline
            list_padding: 2
            separate_lists: true
        - language_pragmas:
            style: vertical
            align: false
            remove_redundant: true
        - trailing_whitespace: {}

      columns: 110
    '';

  };
}
