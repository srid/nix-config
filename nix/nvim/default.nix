# Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  # cf. https://nixos.wiki/wiki/Vim#Adding_new_plugins 

  customPlugins = {
    neovim-ghcid = pkgs.vimUtils.buildVimPlugin {
      name = "ghcid";
      src = (pkgs.fetchFromGitHub { 
        owner = "ndmitchell";
        repo = "ghcid";
        rev = "5d7f859bc6dd553bdf93e6453391353cf310e232";
        sha256 = "1gyasmk6k2yqlkny27wnc1fn2khphgv400apfh1m59pzd9mdgsc2";
      }) + "/plugins/nvim";
    };

    papercolor-theme = pkgs.vimUtils.buildVimPlugin {
      name = "papercolor-theme";
      src = pkgs.fetchFromGitHub {
        owner = "NLKNguyen";
        repo = "papercolor-theme";
        rev = "ddd0986";
        sha256 = "1dhbnd99xs6l5alqhn9m1nynmr9sbvrqj2137l23ysisprl3rgmr";
      };
    };

    indenthaskell = pkgs.vimUtils.buildVimPlugin {
      name = "indenthaskell";
      src = pkgs.fetchFromGitHub {
        owner = "vim-scripts";
        repo = "indenthaskell.vim";
        rev = "17380713774ea4f3ca5da1de455126fa1cce82f7";
        sha256 = "1cs9qkn40fk3c8a9kvbdm3d6izf944awiagpmllkvlb4ci9m6lk7";
      };
    };

    nerdtree = pkgs.vimUtils.buildVimPlugin {
      name = "nerdtree";
      src = pkgs.fetchFromGitHub {
        owner = "scrooloose";
        repo = "nerdtree";
        rev = "e47e588705bd7d205a3b5a60ac7090c9a2504ba2";
        sha256 = "15ai00k7w0brbjvmsj920hpnqy4iz1y3b0pw04m3mlcx20pkfy9s";
      };
    };

    lastpos = pkgs.vimUtils.buildVimPlugin {
      name = "lastpos";
      src = pkgs.fetchFromGitHub {
        owner = "vim-scripts";
        repo = "lastpos.vim";
        rev = "21a22ce4a11117cae8a0017c1cd9a9094fe5adf2";
        sha256 = "0b4xd87a8pxhdf6g8digvjc1a83y572qk4qfdccda2r5m4knidm4";
      };
    };

    ag = pkgs.vimUtils.buildVimPlugin {
      name = "ag";
      src = pkgs.fetchFromGitHub {
        owner = "rking";
        repo = "ag.vim";
        rev = "4a0dd6e190f446e5a016b44fdaa2feafc582918e";
        sha256 = "1dz7rmqv3xw31090qms05hwbdfdn0qd1q68mazyb715cg25r85r2";
      };
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
        { name = "ctrlp-py-matcher"; }
        { name = "papercolor-theme"; }
        { name = "indenthaskell"; }
        { name = "nerdtree"; }
        { name = "lastpos"; }
        { name = "ag"; }
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

