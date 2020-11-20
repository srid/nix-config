# NixOS on Thinkpad P71

{ config, pkgs, ... }:

let 
  hostName = "bornagain";
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  imports =
    [ /etc/nixos/hardware-configuration.nix

      ./p71/hardware.nix
      ./p71/wireguard.nix
      ./p71/postgresql.nix
      ./p71/webapps.nix
      ./p71/nix-serve.nix

      # Disable these, if running as server.
      ./p71/graphics.nix
      ../nixos/gnome.nix

      ../nixos/shell.nix
      ../nixos/tmux.nix
      ../nixos/fonts.nix

      ../nixos/docker.nix
      ../nixos/redshift.nix
      ../nixos/syncthing.nix
      ../nixos/syncthing-tray.nix

      ../nixos/quebec.nix
      ../nixos/nix.nix
      # ../nixos/ci.nix

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
    # Until https://github.com/NixOS/nixpkgs/pull/102106 is merged
    kernelParams = [ "msr.allow_writes=on" ];
  };

  hardware = {
    enableRedistributableFirmware = true;
    enableAllFirmware = true;
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

  security.apparmor.enable = true;
  # To enable lxd, first enable apparmor and reboot
  virtualisation.lxd.enable = true;

  environment.systemPackages = with  pkgs; [
    chromium

    nixpkgs-master.vscode
    # Needed for vscode-remote extension per
    # https://nixos.wiki/wiki/Vscode
    nodejs-10_x

    steam
  ];

  users.extraUsers = {
    srid = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "networkmanager" "audio" "docker" "lxd" "ipfs" ];
      openssh.authorizedKeys.keys = [ 
        (builtins.readFile ../private-config/ssh/id_rsa.pub) 
      ];
      packages = with pkgs; [
        slack
        qmmp  # winamp like
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
