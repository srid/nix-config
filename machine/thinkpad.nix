# NixOS on Thinkpad P71

{ config, pkgs, ... }:

{
  imports =
    [ /etc/nixos/hardware-configuration.nix

      ./thinkpad/hardware.nix
      ./thinkpad/graphics.nix
      ./thinkpad/wireguard.nix
      ./thinkpad/postgresql.nix
      ./thinkpad/webhost.nix
     
      ../nix/base.nix
      ../nix/caches.nix
      ../nix/gui.nix
      ../nix/fonts.nix
      ../nix/dropbox.nix

      # Using GitHub Actions with cachix instead
      # ../nix/ci.nix

      ../private-config
      <home-manager/nixos>
    ];
  home-manager.users.srid = (import ../nix/home.nix "thebeast");

  # EFI boot
  boot = {
    cleanTmpDir = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = false;
    # Always use the latest available kernel.
    kernelPackages = pkgs.linuxPackages_latest;
  };

  # Until nvidia fixes their broken package
  # https://github.com/NixOS/nixpkgs/issues/90459#issuecomment-647041204
  nixpkgs.overlays = [
    (self: super: {
      linuxPackages_latest = super.linuxPackages_latest.extend (self: super: {
        nvidiaPackages = super.nvidiaPackages // {
          stable = super.nvidiaPackages.stable.overrideAttrs (attrs: {
            patches = [
              (pkgs.fetchpatch {
                name = "nvidia-kernel-5.7.patch";
                url = "https://gitlab.com/snippets/1965550/raw";
                sha256 = "03iwxhkajk65phc0h5j7v4gr4fjj6mhxdn04pa57am5qax8i2g9w";
              })
            ];

            passthru = {
              settings = pkgs.callPackage (import <nixpkgs/pkgs/os-specific/linux/nvidia-x11/settings.nix> self.nvidiaPackages.stable "15psxvd65wi6hmxmd2vvsp2v0m07axw613hb355nh15r1dpkr3ma") {
                withGtk2 = true;
                withGtk3 = false;
              };

              persistenced = pkgs.lib.mapNullable (hash: pkgs.callPackage (import <nixpkgs/pkgs/os-specific/linux/nvidia-x11/persistenced.nix> self.nvidiaPackages.stable hash) { }) "13izz9p2kg9g38gf57g3s2sw7wshp1i9m5pzljh9v82c4c22x1fw";
            };
          });
        };
      });
    })
  ];

  networking = {
    hostName = "thebeast";
    networkmanager.enable = true;
  };

  services.openssh = {
    enable = true;
    ports = [22];
  };

  services.netdata = {
    enable = true;
  };

  services.ipfs = {
    # Not using it due to issues with publishing static sites
    enable = false;
    autoMount = true;
  };

  virtualisation.docker.enable = false;
  virtualisation.lxd.enable = true;

  environment.systemPackages = with  pkgs; [
    docker-compose

    # TODO: Use autorandr to switch between modes.
    # For now, doing it manually using arandr.
    #   Using .screenlayout/lgonly.sh when connecting the monitor
    arandr
    autorandr
    acpi
    pulsemixer
    pavucontrol  # GUI version of pulsemixer
    blueman
    ntfs3g

    inkscape
  ];

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "audio" "docker" "lxd" "ipfs" ];
    shell = pkgs.fish;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
