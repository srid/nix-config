{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      ./nix/base.nix
      ./nix/emacs.nix
      # ./nix/gui.nix
      ./nix/dev.nix
    ];

  # GRUB
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.growPartition = true;  # Automatically grow root volume to max size; then we must manually use `resize2fs` to do the actual resize.

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "machambre";

  services.openssh.enable = true;

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "audio" ];
    shell = pkgs.fish;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?
}
