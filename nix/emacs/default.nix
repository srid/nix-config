{ pkgs, ... }:
let
  sources = import ../sources.nix;
  doom-emacs = pkgs.callPackage sources.nix-doom-emacs {
    # Doom configuration
    doomPrivateDir = ./doom.d; 
  };
in {
  home.packages = [ doom-emacs ];
  home.file.".emacs.d/init.el".text = ''
     (load "default.el")
  '';
}

