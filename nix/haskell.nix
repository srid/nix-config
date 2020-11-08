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

    # For this shit: https://github.com/haskell/haskell-language-server/issues/171#issuecomment-647480950
    stack

    # pkgs.vscode
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
