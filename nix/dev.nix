{ config, pkgs, ...}:

{
  imports = [ ./gotty.nix ];

  environment.systemPackages = with pkgs; [
    haskellPackages.stylish-haskell
    python

    emacs26
    asciinema
    tmate
  ];
}
