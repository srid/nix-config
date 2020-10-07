# NixOS on Thinkpad P71

{ config, pkgs, ... }:

let 
  hostName = "bornagain";
in {
  imports =
    [ /etc/nixos/hardware-configuration.nix

      ./p71/hardware.nix
      ./p71/graphics.nix
      ./p71/wireguard.nix
      ./p71/postgresql.nix
      ./p71/webapps.nix
      ./p71/nix-serve.nix

      ../nixos/tmux.nix
      ../nixos/gnome.nix
      ../nixos/fonts.nix
      ../nixos/dropbox.nix
      ../nixos/syncthing-firewall.nix

      ../nixos/quebec.nix
      ../nixos/nix.nix
      ../nixos/ci.nix

      <home-manager/nixos>
    ];

  home-manager.users.srid = (import ../nix/home.nix { 
    inherit pkgs config hostName;
  } );

  # EFI boot
  boot = {
    cleanTmpDir = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = false;
    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking = {
    inherit hostName;
    networkmanager.enable = true;
    wireless.networks = ./private-config/wifi.nix;
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ 22 ];
    };
  };

  services.openssh = {
    enable = true;
    ports = [22];
  };

  virtualisation.docker.enable = false;
  security.apparmor.enable = true;
  # To enable lxd, first enable apparmor and reboot
  virtualisation.lxd.enable = true;

  environment.systemPackages = with  pkgs; [
    acpi
    ntfs3g
  ];

  users.extraUsers = {
    srid = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "networkmanager" "audio" "docker" "lxd" "ipfs" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [ 
        (builtins.readFile ../private-config/ssh/id_rsa.pub) 
      ];
    };
    apps = {
      uid = 1001;
      isNormalUser = true;
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
