{ pkgs, ... }:
let
 doom-emacs = pkgs.callPackage (builtins.fetchTarball {
   url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
 }) {
   # Doom configuration
   doomPrivateDir = ./emacs/doom.d; 
 };
in {
 home.packages = [ doom-emacs ];
 home.file.".emacs.d/init.el".text = ''
     (load "default.el")
 '';
}

