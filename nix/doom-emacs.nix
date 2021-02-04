{ pkgs, ... }:
let
  # TODO: nix-thunk
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom-emacs/doom.d;  # Directory containing your config.el init.el
  };
in {
  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };
  home.file.".emacs.d/init.el".text = ''
      (load "default.el")
  '';

  # Packages used by use-package of init.el
  home.packages = with pkgs; [
    ormolu
  ];

  services.emacs = {
    enable = true;
  };
}
