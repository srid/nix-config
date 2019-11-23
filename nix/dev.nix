{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    python

    emacs26
    asciinema
    tmate
  ];
}
