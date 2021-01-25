{ pkgs, ... }:

let
  # TODO: nix-thunk
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom-emacs/doom.d;  # Directory containing your config.el init.el
                                # and packages.el files
  };
in {
  # home.packages = [ doom-emacs ];
  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };
  home.file.".emacs.d/init.el".text = ''
      (load "default.el")
  '';
}
