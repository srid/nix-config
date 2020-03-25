# Just a bare configuration.nix for use with LXD
# https://www.srid.ca/2012301.html#running-nixos-in-lxd
{ config, pkgs, ... }:

{
  imports =
    [ /etc/nixos/hardware-configuration.nix
    ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  environment.systemPackages = [ pkgs.tmux ];

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.bash;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
