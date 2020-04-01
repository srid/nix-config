# Just a bare configuration.nix for use with LXD
# https://www.srid.ca/2012301.html#running-nixos-in-lxd
#
# Known issues:
#  - https://github.com/nix-community/nixos-generators/issues/42
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

  # Allow SSH based login
  services.openssh = {
    enable = true;
    ports = [22];
  };

  networking = {
    # Allow the DHCP server to provide a hostname automatically.
    hostName = ""; 
    # On my system eth0 is the interface used by the LXD container. YMMV.
    interfaces.eth0.useDHCP = true;
  };

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.bash;
    openssh.authorizedKeys.keyFiles = [ ../keys/nixos.pub ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  # copy the configuration.nix into /run/current-system/configuration.nix
  system.copySystemConfiguration = true;
}
