{ pkgs, ... }:

let
  ormoluSrc = pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "683cbea";
    sha256 = "12z3qfs5nq3pgxwlr6gsmgqm363z0k9xac9bhikm5mi85b992yi4";
  };

  # TODO: configure cache in home-manager first; until then, on macOS, use
  # 'cachix use hercules-ci' before 'home-manager switch'
  ghcideNixSrc = pkgs.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "c940edd";
    sha256 = "01f2x5sgncd468h99w3mpkkb1203akachm12czmiwbvgishf7dwp";
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
    # ghcide : disabling, because uttery broken and unreliable
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
