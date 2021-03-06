{ config, lib, pkgs, ... }:

{
  # https://github.com/nix-community/emacs-overlay
  # TODO: Make this a channel?
  nixpkgs.overlays = [
    (import (import ../dep/emacs-overlay/thunk.nix))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackages (epkgs: [
      epkgs.emacs-libvterm
    ]);
  };

  home.packages = with pkgs; [
    # Packages used by use-package of init.el
    ormolu
    sqlite

    # For doom-emacs to compile vterm
    # https://github.com/hlissner/doom-emacs/issues/2065
    cmake
    gcc
    libtool
  ];

  # Not using this, yet.
  services.emacs = {
    enable = false;
    # package = pkgs.emacs;
  };
}
