{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./nix/base.nix
      ./nix/emacs.nix
      ./nix/gui.nix
      ./nix/dev.nix
    ];

  # EFI boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "thebeast"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  services.openssh.enable = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  # DPI
  services.xserver.dpi = 200;
  # Nvidia (is it enabled in BIOS?)
  services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.bumblebee.enable = true;
  # hardware.bumblebee.connectDisplay = true;

  # Xmonad multi monitor setup works better with Plasma.
  # Use lightdm so I can selecet xmonad+plasma as the session.
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.lightdm.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
