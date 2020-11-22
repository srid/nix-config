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
      <home-manager/nixos>

      # Essential imports
      ../nixos/nix.nix
      ../nixos/quebec.nix
      ../nixos/shell.nix
      ../nixos/tmux.nix

      # Other imports
      ../nixos/fonts.nix
      ../nixos/gnome.nix
      ../nixos/i3.nix
      ../nixos/redshift.nix
      ../nixos/syncthing.nix
      ../nixos/syncthing-tray.nix
      
      # Remote building (because X1C7 is not the fastest)
      ../nixos/nix-distributed.nix

      ../private-config/caches.nix

      ./x1c7/custom-kernel.nix
      ./x1c7/brightness.nix
      ./x1c7/wireguard.nix
      ./x1c7/touchpad-trackpoint.nix
      ./x1c7/suspend-crash-workaround.nix
    ];

  home-manager.users.srid = (import ../nix/home.nix {
    inherit pkgs config hostName;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking.hostName = hostName;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # wifi workarounds for unreliability
  /*
  networkmanager = {
    enable = true;
    wifi.powersave = false;
    wifi.scanRandMacAddress = false;
  };
  */

  # See also nix/ssh.nix
  programs.ssh = {
    startAgent = true;
  };

  programs.light.enable = true;

  # Allow non-free firmware, such as for intel wifi
  hardware = {
    enableRedistributableFirmware = true;
  };

  # Sound is trickly. Use alsamixer, and make sure that laptop card is not
  # muted. Use pulsemixer to switch output device when docking/detaching.
  sound.enable = true;

  services.fwupd.enable = true;
  # typing password is faster than fingerprint touch; leave disabled
  #services.fprintd.enable = true;
  #security.pam.services.login.fprintAuth = true;
  #security.pam.services.xscreensaver.fprintAuth = true;

  users.users.srid = {
     isNormalUser = true;
     extraGroups = [ "wheel" "docker" ];
     shell = pkgs.bash;
     packages = with pkgs; [
       slack
       chromium
       vscode
       qmmp
       mpv
       peek
       pciutils
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

