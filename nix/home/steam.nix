{ config, lib, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
in
{
  home.packages = with pkgs; [
    unstable.steam
  ];
}
