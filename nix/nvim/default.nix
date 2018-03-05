{ pkgs ? import <nixpkgs-unstable> {} }:

let
  customPlugins = {
    papercolor-theme = pkgs.vimUtils.buildVimPlugin {
      name = "papercolor-theme";
      src = pkgs.fetchFromGitHub {
        owner = "NLKNguyen";
        repo = "papercolor-theme";
        rev = "58ad6d94c39bf0a2779aa2f6e0a34712d407964f";
        sha256 = "1ifscyvna7ip67xd3yd30a0db96malg17k5i40wh2sxd6qadmcyv";
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
  };
in
  with pkgs; neovim.override {
    configure = {
      customRC = ''
        "Colorscheme
        "-------------------------
        set background=light
        "let base16colorspace=256
        "colorscheme base16-mexico-light
        colorscheme PaperColor
        "Adjust theme search colors
        hi Search ctermbg=250 ctermfg=240
        hi Comment ctermfg=245
      '';

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
        { name = "vim-nix"; }
        { name = "haskell-vim"; }
        { name = "vim-gitgutter"; }
        { name = "ctrlp"; }
        { name = "ctrlp-py-matcher"; }
        { name = "papercolor-theme"; }
        { name = "indenthaskell"; }
        { name = "nerdtree"; }
        # { name = "neocomplcache"; }
        # { name = "papercolor-theme"; }
      ];
    };
  }

