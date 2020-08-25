{ pkgs, ... }:

let
  # TODO: configure cache in home-manager first; until then, on macOS, use
  # 'cachix use hercules-ci' before 'home-manager switch'
  ghcideNixSrc = pkgs.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "67493b8";
    sha256 = "sha256:1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
  };
in {
  home.packages = with pkgs.haskellPackages; [

    # Some commonly used tools
    cachix
    pandoc
    hlint

    hoogle
    ormolu
    ghcide

    # ghcide
    # (import ghcideNixSrc {}).ghcide-ghc882
  ];

  home.file = {
    # ghci
    ".ghci".text = ''
      :set prompt "λ> "
    '';

    # coc.vim
    ".config/nvim/coc-settings.json".text = ''
      {
        "languageserver": {
          "neuron": {
            "command": "neuron-language-server",
            "filetypes": ["markdown"]
          },
          "haskell": {
            "command": "ghcide",
            "args": [
              "--lsp"
            ],
            "rootPatterns": [
              ".stack.yaml",
              ".hie-bios",
              "BUILD.bazel",
              "cabal.config",
              "package.yaml"
            ],
            "filetypes": [
              "hs",
              "lhs",
              "haskell"
            ]
          }
        }
      }
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
