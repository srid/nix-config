# A bare configuration.nix, with LXC specific stuff, for use with LXD
# https://www.srid.ca/lxc-nixos
#
# To initialize the container:
#   - cp /run/current-system/configuration.nix /etc/nixos/
#   - nixos-rebuild switch
{ config, lib, pkgs, modulesPath, ... }:

let
  sridKey =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYQ003p7fB5ICQehLwhDBomY9WzkNBeijkSw9ADGU+ECrPakeIH3pntUWRJH1W93vKnLqpkn6HLGEXD9MCR0s98uhh8hT7uAYCxQTbEeKT3PYkfz3oe7XaR8rE601sds0ZyFwH7l8cvK97pGr+uhFXAaohiV6VqmLVXhManEjZZ8GfYWBD9BCmIJk43G3OGa5QYFeHqztprXaJNU5dFPv2Uq2C+L6EvfCfkK2OO1BLZgL+Rai5jjyy6k0fcfsxxd9BdGUwqDhcBeyTIzX9rePMugf/xD+6uNRxTU+vjVpGUtFOw6rpgmVyFv9mn3QMNdQBc5hYKVbIQwMNGTzGgcQv srid@nixos";
in
{
  imports =
    [ # add a copy of nixpkgs to the image
      "${modulesPath}/installer/cd-dvd/channel.nix"
    ];


  # The majority of this indented snippet comes from
  # https://github.com/NixOS/nixpkgs/issues/9735#issuecomment-500164017

    # it is not perfect because `boot.isContainer` means NixOS containers
    # not LXC, but let have something to start with
    boot.isContainer = true;

    # `boot.isContainer` implies NIX_REMOTE = "daemon"
    # (with the comment "Use the host's nix-daemon")
    # Our host is Ubuntu, so we do not expect any "host's nix-daemon"
    environment.variables.NIX_REMOTE = lib.mkForce "";

    # (optional) suppress daemons which will vomit to the log about their unhappiness
    systemd.services."console-getty".enable = false;
    systemd.services."getty@"       .enable = false;

    # the key point (need https://github.com/NixOS/nixpkgs/issues/62856 be fixed)
    system.build.installBootLoader = pkgs.writeScript "installBootLoader.sh" ''
      #!${pkgs.bash}/bin/bash

      export TOPLEVEL="$1"
      echo "=== installBootLoader TOPLEVEL=$TOPLEVEL"

      ${pkgs.coreutils}/bin/mkdir -p /sbin
      ${pkgs.coreutils}/bin/rm -rf /sbin/init || true     # there could be symlink to "/lib/systemd/systemd"

      ${pkgs.coreutils}/bin/cat > /sbin/init <<EOF
      #!${pkgs.bash}/bin/bash

      # lustrate old OS here (otherwise ruins of /etc would prevent NixOS to boot properly)
      if [ -e "/etc/debian_version" -o -e "/etc/redhat-release" -o -e "/etc/arch-release" -o -e "/etc/gentoo-release" ]; then
        ${pkgs.coreutils}/bin/rm -rf /bin            || true
        ${pkgs.coreutils}/bin/rm -rf /etc            || true
        ${pkgs.coreutils}/bin/rm -rf /lib            || true
        ${pkgs.coreutils}/bin/rm -rf /lib64          || true
        ${pkgs.coreutils}/bin/rm -rf /snap           || true
        ${pkgs.coreutils}/bin/rm -rf /usr            || true
        ${pkgs.coreutils}/bin/rm -rf /var            || true
      fi

      exec $TOPLEVEL/init
      EOF

      ${pkgs.coreutils}/bin/chmod 0755 /sbin/init

      # LXC: two replaces (in LXC container /dev/net/tun is pre-available, "dev-net-tun.device" always fails)
      substituteInPlace nixos/modules/tasks/network-interfaces-scripted.nix \
        --replace '[ "dev-net-tun.device" ' \
                  'optionals (!config.boot.isContainer) [ "dev-net-tun.device" ] ++ [ '

      # LXC: fix "Failed to mount Kernel Configuration File System." on "nixos-rebuild switch"
      substituteInPlace nixos/modules/system/boot/systemd.nix \
        --replace '"sys-kernel-config.mount"'         '] ++ (optional (!config.boot.isContainer) "sys-kernel-config.mount"      ) ++ [' \
        --replace '"systemd-journald-audit.socket"'   '] ++ (optional (!config.boot.isContainer) "systemd-journald-audit.socket") ++ ['
    '';

  fileSystems."/" = 
    { device = "/var/lib/lxd/disks/default.img";
      fsType = "btrfs";
    };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  environment.systemPackages = with pkgs; [ 
    tmux
    neovim
  ];

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
    shell = pkgs.bash;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      sridKey
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

  # copy the configuration.nix into /run/current-system/configuration.nix
  system.copySystemConfiguration = true;
}
