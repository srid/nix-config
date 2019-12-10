{ pkgs, fetchGH, ... }:

let
  ormolu = fetchGH "tweag/ormolu" "46ede8a";
  # all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  # https://github.com/haskell/cabal/issues/4739#issuecomment-359209133
  macOSCaseNameFix = drv:
    pkgs.haskell.lib.appendConfigureFlag drv "--ghc-option=-optP-Wno-nonportable-include-path";
in {
  home.packages = with pkgs.haskellPackages; [
    # Some commonly used tools
    cachix
    pandoc

    # hoogle used by emacs
    hoogle
    # ormolu code formatter
    (macOSCaseNameFix (callPackage ormolu { inherit pkgs; }).ormolu)

    # stylish-hashell code formatter
    stylish-haskell
    # Install stable HIE for specified GHC versions
    # (all-hies.selection { selector = p: { inherit (p) ghc865 ghc864; }; })
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
