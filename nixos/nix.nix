{ config, pkgs, ... }:
{
  nix.trustedUsers = [ "root" "srid" ];
  nixpkgs.config.allowUnfree = true;
}
