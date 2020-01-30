# Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

{ pkgs ? import <nixpkgs> {}, fetchGH, ... }:

let
  # cf. https://nixos.wiki/wiki/Vim#Adding_new_plugins 

  customPlugins = {
    neovim-ghcid = pkgs.vimUtils.buildVimPlugin {
      name = "ghcid";
      src = (fetchGH "ndmitchell/ghcid" "dfa37af") + "/plugins/nvim";
    };
    indenthaskell = pkgs.vimUtils.buildVimPlugin {
      name = "indenthaskell";
      src = fetchGH "vim-scripts/indenthaskell.vim" "17380713774ea4f3ca5da1de455126fa1cce82f7";
    };
    lastpos = pkgs.vimUtils.buildVimPlugin {
      name = "lastpos";
      src = fetchGH "vim-scripts/lastpos.vim" "21a22ce4a11117cae8a0017c1cd9a9094fe5adf2";
    };
  };
in
  with pkgs; neovim.override {
    configure = {
      # Builtin packaging
      # List of plugins: nix-env -qaP -A nixos.vimPlugins
      packages.myVimPackage = with pkgs.vimPlugins; {
        # Loaded on launch
        start = [ ];
        # Manually loadable by calling `:packadd $plugin-name
        opt = [ ];
      };

      # VAM
      vam.knownPlugins = pkgs.vimPlugins // customPlugins;
      vam.pluginDictionaries = [
        { name = "goyo"; }  # Distraction free writing
        { name = "vim-auto-save"; }
        { name = "vim-nix"; }
        { name = "haskell-vim"; }
        { name = "vim-gitgutter"; }
        { name = "ctrlp"; }
        { name = "papercolor-theme"; }
        { name = "indenthaskell"; }
        { name = "nerdtree"; }
        { name = "lastpos"; }
        { name = "vim-nix"; }
        { name = "fugitive"; }
        { name = "tslime"; }
        { name = "fzf-vim"; }
        { name = "fzfWrapper"; }
        { name = "neovim-ghcid"; }
        { name = "coc-nvim"; }
        { name = "vim-airline"; }
        # { name = "vim-stylish-haskell"; }
      ];

      customRC = builtins.readFile ./config.vim;
    };
  }
