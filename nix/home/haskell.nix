{ pkgs, fetchGH, ... }:

let
  ormoluSrc = fetchGH "tweag/ormolu" "55d8b7f";

  # TODO: configure cache in home-manager first; until then, on macOS, use
  # 'cachix use hercules-ci' before 'home-manager switch'
  ghcideNixSrc = fetchGH "hercules-ci/ghcide-nix" "c940edd";

  # https://github.com/haskell/cabal/issues/4739#issuecomment-359209133
  macOSCaseNameFix = drv:
    pkgs.haskell.lib.appendConfigureFlag drv "--ghc-option=-optP-Wno-nonportable-include-path";
in {
  home.packages = with pkgs.haskellPackages; [
    # Some commonly used tools
    cachix
    pandoc

    hoogle

    # ormolu code formatter
    (macOSCaseNameFix (callPackage ormoluSrc { }).ormolu)
    # ghcide
    (import ghcideNixSrc {}).ghcide-ghc865
  ];

  home.file = {
    # ghci
    ".ghci".text = ''
      :set prompt "λ> "
    '';
    # stylish-haskell (obsidian style)
    # I now use ormolu; retaining this config for legacy purposes.
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
