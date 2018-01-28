with import <nixpkgs> {};

with pkgs; neovim.override {
  configure = {
    customRC = ''
      syntax enable
      set colorcolumn=80
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
      # { name = "vim-pandoc"; }
      # { name = "vim-pandoc-syntax"; }
    ]; 
  };
}
