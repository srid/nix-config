{ pkgs, ... }:

let
  ormoluSrc = pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "409c1f9aa8064ffc89aa8d047181ffee95e4fc46";
    sha256 = "sha256:0j26r42nw854wn9zyn1dm1h87rjdjwfdyncjgp8fj1q19hnz99gw";
  };

  # TODO: configure cache in home-manager first; until then, on macOS, use
  # 'cachix use hercules-ci' before 'home-manager switch'
  ghcideNixSrc = pkgs.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "67493b8";
    sha256 = "sha256:1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
  };

  # https://github.com/haskell/cabal/issues/4739#issuecomment-359209133
  macOSCaseNameFix = drv:
    pkgs.haskell.lib.appendConfigureFlag drv "--ghc-option=-optP-Wno-nonportable-include-path";
in {
  home.packages = with pkgs.haskellPackages; [

    # Some commonly used tools
    cachix
    pandoc
    hlint

    hoogle

    # ormolu code formatter
    (macOSCaseNameFix (import ormoluSrc { }).ormolu)
    # ghcide
    # (import ghcideNixSrc {}).ghcide-ghc882
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
