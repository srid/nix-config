{ pkgs, ... }:

{
  home.packages = with pkgs.haskellPackages; [

    # Some commonly used tools
    cachix
    pandoc
    hlint

    hoogle
    ormolu
    ghcide

    pkgs.vscode
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
  };
}
