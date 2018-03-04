with import <nixpkgs-unstable> {};

let
  customPlugins.papercolor-theme = pkgs.vimUtils.buildVimPlugin {
      name = "papercolor-theme";
      src = pkgs.fetchFromGitHub {
        owner = "NLKNguyen";
        repo = "papercolor-theme";
        rev = "58ad6d94c39bf0a2779aa2f6e0a34712d407964f";
        sha256 = "1ifscyvna7ip67xd3yd30a0db96malg17k5i40wh2sxd6qadmcyv";
      };
  };
in (
  with pkgs; neovim.override {
    configure = {
      customRC = ''
        syntax enable
        filetype plugin indent on
        imap fd <Esc>

        " Haskell
        set tabstop=8                   "A tab is 8 spaces
        set expandtab                   "Always uses spaces instead of tabs
        set softtabstop=4               "Insert 4 spaces when tab is pressed
        set shiftwidth=4                "An indent is 4 spaces
        set shiftround                  "Round indent to nearest shiftwidth multiple

        set nocompatible
        " XXX NeoBundleFetch 'Shougo/neobundle.vim'

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
        # { name = "neocomplcache"; }
        # { name = "papercolor-theme"; }
      ];
    };
  }
)
