# Just a bare configuration.nix for use with LXD
# https://www.srid.ca/2012301.html#running-nixos-in-lxd
#
# Known issues:
#  - https://github.com/nix-community/nixos-generators/issues/42
#
# To initialize the container:
#   - cp /run/current-system/configuration.nix /etc/nixos/
#   - nixos-rebuild switch -I nixpkgs=channel:nixos-20.03
#   - TODO: things are still broken at this stage
{ config, pkgs, modulesPath, ... }:

{
  imports =
    [ # add a copy of nixpkgs to the image
      "${modulesPath}/installer/cd-dvd/channel.nix"
    ];

  boot = {
    loader.grub = {
      enable = false;
    };
  };

  fileSystems."/" = 
    { device = "/var/lib/lxd/disks/default.img";
      fsType = "btrfs";
    };

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
    # On my system eth0 is the interface used by the LXD container. YMMV.
    interfaces.eth0.useDHCP = true;
  };

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.bash;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYQ003p7fB5ICQehLwhDBomY9WzkNBeijkSw9ADGU+ECrPakeIH3pntUWRJH1W93vKnLqpkn6HLGEXD9MCR0s98uhh8hT7uAYCxQTbEeKT3PYkfz3oe7XaR8rE601sds0ZyFwH7l8cvK97pGr+uhFXAaohiV6VqmLVXhManEjZZ8GfYWBD9BCmIJk43G3OGa5QYFeHqztprXaJNU5dFPv2Uq2C+L6EvfCfkK2OO1BLZgL+Rai5jjyy6k0fcfsxxd9BdGUwqDhcBeyTIzX9rePMugf/xD+6uNRxTU+vjVpGUtFOw6rpgmVyFv9mn3QMNdQBc5hYKVbIQwMNGTzGgcQv srid@nixos"
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

  # copy the configuration.nix into /run/current-system/configuration.nix
  system.copySystemConfiguration = true;
}
