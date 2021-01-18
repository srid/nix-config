# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let 
  hostName = "thebeast";
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix

      <nixos-hardware/lenovo/thinkpad>
      # Assuming on blind faith that is okay for p71
      <nixos-hardware/lenovo/thinkpad/p53>
      # Pull in hardware specific modules when relevant
      <nixos-hardware/common/pc/ssd>

      <home-manager/nixos>

      ../nixos/nix.nix
      ../nixos/quebec.nix
      ../nixos/fucking-basics.nix
      ../nixos/swap-ctrl-caps.nix
      ../nixos/shell.nix
      ../nixos/tmux.nix
      ../nixos/syncthing.nix
      ../nixos/server-mode.nix
      ../nixos/vscode.nix
      ../nixos/passwordstore.nix

      ./thinkpad/touchpad-trackpoint.nix
      ./display/brightness.nix
      ./display/lg-ultrafine-5k/hidpi.nix
      ./display/lg-ultrafine-5k/hidpi-xorg.nix
      ../nixos/xmonad.nix
      ../nixos/redshift.nix
      ../nixos/taffybar.nix
      # ../nixos/autolock.nix
      ../nixos/fonts.nix

      ./p71/graphics.nix

      ../private-config/caches.nix
    ];

  # Lon

  home-manager.users.srid = (import ../nixos/home.nix { 
    inherit pkgs config hostName;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # Until https://github.com/NixOS/nixpkgs/pull/102106 is merged
  boot.kernelParams = [ "msr.allow_writes=on" ];

  hardware = {
    enableRedistributableFirmware = true;
    enableAllFirmware = true;

    # Audio
    # Use `pactl set-sink-volume 0 +10%` to increase volume.
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      daemon.config = {
        flat-volumes = "no";
      };
    };

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    # Bluetooth
    # https://nixos.wiki/wiki/Bluetooth
    bluetooth = {
      enable = true;
      # For Bose QC 35
      config = {
        General = {
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
  services.blueman.enable = true;

  networking = { 
    inherit hostName;
    networkmanager.enable = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  #services.xserver.enable = false;
  #services.xserver.displayManager.gdm.enable = true;
  #services.xserver.desktopManager.gnome3.enable = true;

  systemd.services = let 
    neuronSrv = description: notesDir: port: import ../nixos/neuron.nix { 
      inherit pkgs description notesDir port; 
    };
  in {
    neuron-zk = neuronSrv "neuron-zk" "/home/srid/Documents/zk" 9000; 
  };


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.srid = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "ipfs" ]; 
  };

  environment.systemPackages = with pkgs; [
    google-chrome
    signal-desktop
    brave
    pulsemixer
    mpv
    radicle-upstream
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

