{ config, lib, pkgs, ... }:

{
  # https://github.com/nix-community/emacs-overlay
  # TODO: Make this a channel?
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Packages used by use-package of init.el
  home.packages = with pkgs; [
    ormolu
    sqlite

    # For doom-emacs to compile vterm
    # https://github.com/hlissner/doom-emacs/issues/2065
    cmake
    gcc
    libtool
  ];

  services.emacs = {
    enable = false;
    package = pkgs.emacs;
  };
}
