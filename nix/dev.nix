{ config, pkgs, ...}:

{
  imports = [ ./gotty.nix ];

  environment.systemPackages = with pkgs; [
    python

    emacs26
    asciinema
    tmate
  ];
}
