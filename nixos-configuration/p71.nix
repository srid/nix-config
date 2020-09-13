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
     
      ../nixos/caches.nix
      ../nixos/gui.nix
      ../nixos/gnome.nix
      #../nixos/kde.nix
      #../nixos/i3.nix
      ../nixos/google-chrome.nix
      ../nixos/fonts.nix
      ../nixos/dropbox.nix
      ../nixos/syncthing-firewall.nix

      # Using GitHub Actions with cachix instead
      # ../nix/ci.nix

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
    # Always use the latest available kernel. Disabled, because buggy (crashes)
    kernelPackages = pkgs.linuxPackages_latest;
  };

  time.timeZone = "America/New_York";

  nix.trustedUsers = [ "root" "srid" ];
  nixpkgs.config.allowUnfree = true;

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
  #security.apparmor.enable = true;
  #virtualisation.lxd.enable = true;

  environment.systemPackages = with  pkgs; [
    acpi
    pulsemixer
    ntfs3g
  ];

  users.extraUsers = {
    srid = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "networkmanager" "audio" "docker" "lxd" "ipfs" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [ (builtins.readFile ../private-config/ssh/id_rsa.pub) ];
    };
    apps = {
      uid = 1001;
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
