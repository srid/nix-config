{ config, pkgs, ... }:

let 
  hostName = "bebe";
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix

      # Get community hardware configuration
      <nixos-hardware/lenovo/thinkpad>
      <nixos-hardware/lenovo/thinkpad/x1>
      <nixos-hardware/lenovo/thinkpad/x1/7th-gen>

      # home-manager
      ((import ../dep/home-manager/thunk.nix) + "/nixos")

      ./thinkpad/touchpad-trackpoint.nix
      ./display/brightness.nix
      ./display/lg-ultrafine-5k/hidpi.nix
      ./display/plug-n-play.nix

      # Essential imports
      ../nixos/nix.nix
      ../nixos/quebec.nix
      ../nixos/tmux.nix
      ../nixos/passwordstore.nix
      ../nixos/protonvpn.nix
      ../nixos/docker.nix

      # Other imports
      ../nixos/fucking-basics.nix
      ../nixos/swap-ctrl-caps.nix
      ../nixos/fonts.nix

      ../nixos/gnome.nix
      #../nixos/xmonad.nix
      #../nixos/taffybar.nix
      #../nixos/autolock.nix
      #../nixos/redshift.nix

      ../nixos/syncthing.nix
      ../nixos/syncthing-tray.nix
      ../nixos/passwordstore.nix

      # Remote building (because X1C7 is not the fastest)
      # ../nixos/nix-distributed.nix

      ../private-config/caches.nix

      # ./x1c7/custom-kernel.nix
      ./x1c7/wireguard.nix
      ./x1c7/audio.nix
      # ./x1c7/suspend-crash-workaround.nix

      # ipfs-cluster
      # (builtins.fetchurl "https://raw.githubusercontent.com/NixOS/nixpkgs/70f514bb3d38c93922a700e120ee8bff08a81c47/nixos/modules/services/network-filesystems/ipfs-cluster.nix")
    ];

  home-manager.users.srid = (import ../nix/home.nix {
    inherit pkgs config hostName;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  swapDevices = [
    { device = "/swapfile";
      priority = 0;
      size = 16384;
    }
  ];

  networking.hostName = hostName;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # USB WiFI adapter: https://www.srid.ca/rtl8821cu.html
  # boot.extraModulePackages = [ config.boot.kernelPackages.rtl8821cu ];
  # networking.interfaces.wlp0s20f0u3.useDHCP = true;  # Not sure if this is required

  networking.networkmanager = {
    enable = true;
    wifi.powersave = false;
    wifi.scanRandMacAddress = false;
  };

  # See also nix/ssh.nix
  programs.ssh = {
    startAgent = true;
  };

  hardware = {
    # Allow non-free firmware, such as for intel wifi
    enableRedistributableFirmware = true;
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  services.openssh.enable = true;

  services.fwupd.enable = true;

  services.ipfs = {
    enable = true;
    enableGC = true;
  };

  # Use this only in GNOME/gdm
  #services.fprintd.enable = true;
  #security.pam.services.login.fprintAuth = true;
  #security.pam.services.xscreensaver.fprintAuth = true;
  #security.pam.services.xlock.fprintAuth = true;

  users.users.srid = {
     isNormalUser = true;
     extraGroups = [ "wheel" "audio" "docker" ];
     shell = pkgs.powershell;
     packages = with pkgs; [
       slack
       brave
       epiphany
       signal-desktop
       tdesktop
       qmmp
       mpv
       peek
       python
       albert
       obsidian
       dotnet-sdk_5
     ];
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

