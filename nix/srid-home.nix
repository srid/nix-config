{ config, pkgs, ...}:

{
  imports = [
    <home-manager/nixos>
  ];
  home-manager.users.srid = (import ./home.nix);
}
