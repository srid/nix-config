with import <nixpkgs> {};

with pkgs; neovim.override {
  configure = {
    customRC = ''
      syntax enable
      filetype plugin indent on
      imap fd <Esc>
    '';

    # Builtin packaging
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
