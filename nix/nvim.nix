with import <nixpkgs> {};

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
    vam.knownPlugins = pkgs.vimPlugins;
    vam.pluginDictionaries = [
      { name = "goyo"; }  # Distraction free writing
      { name = "vim-nix"; }
      { name = "haskell-vim"; }
      { name = "vim-gitgutter"; }
    ]; 
  };
}
