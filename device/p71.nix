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

      ((import ../dep/home-manager/thunk.nix) + "/nixos")

      ../nixos/nix.nix
      ../nixos/quebec.nix
      ../nixos/fucking-basics.nix
      ../nixos/swap-ctrl-caps.nix
      ../nixos/tmux.nix
      ../nixos/syncthing.nix
      ../nixos/server-mode.nix
      ../nixos/passwordstore.nix
      ../nixos/fonts.nix
      ../nixos/protonvpn.nix
      ../nixos/docker.nix

      # Graphics
      ./thinkpad/touchpad-trackpoint.nix
      ./display/brightness.nix
      ./display/lg-ultrafine-5k/hidpi.nix
      ./display/lg-ultrafine-5k/hidpi-xorg.nix
      #../nixos/gnome.nix
      #../nixos/xmonad.nix
      #../nixos/redshift.nix
      #../nixos/taffybar.nix
      # ../nixos/autolock.nix
      #./p71/graphics.nix
      ./p71/wireguard.nix

      ../private-config/caches.nix
    ];

  # Lon

  home-manager.users.srid = (import ../nix/home.nix { 
    inherit pkgs config hostName;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # Until https://github.com/NixOS/nixpkgs/pull/102106 is merged
  boot.kernelParams = [ "msr.allow_writes=on" ];
  # If xserver is not enabled, the default for this goes down. Increase it here. VSCode requires it. 
  # https://code.visualstudio.com/docs/setup/linux#_visual-studio-code-is-unable-to-watch-for-file-changes-in-this-large-workspace-error-enospc
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;

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
      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
  services.blueman.enable = true;
  services.openssh.enable = true;

  services.ipfs = {
    enable = true;
    user = "srid"; # TODO: undo
    # Expose, for wireguard
    # gatewayAddress = "/ip4/0.0.0.0/tcp/8080";
  };

  networking = { 
    inherit hostName;
    networkmanager.enable = true;
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22 ];
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  systemd.services =  
    let 
      obelisk = import ../nix/obelisk.nix { inherit pkgs; };
    in {
      "emanote-Documents-zk" = 
        obelisk.obeliskService 
          "emanote-Documents-zk" "7102" "srid" (import ../dep/emanote {});
      "emanote-Documents-oldzk" = 
        obelisk.obeliskService 
          "emanote-Documents-oldzk" "7103" "srid" (import ../dep/emanote {});
    };

  virtualisation.lxd.enable = true;

  # 1. Login as root, and launch tmux
  # 2. Connect to VPN: `protonvpn c --fastest` (Setup once with `protonvpn init`)
  # 3. Login as user: `su - user`
  # 4. ...
  # 5. Profit!1!!
  containers.vpn = { 
    config =  { config, pkgs, ... }: {
      environment.systemPackages = with pkgs; [
        protonvpn-cli
        youtube-dl
        aria
        tmux
      ];
      users.extraUsers.user = {
        isNormalUser = true;
        uid = 1000;
        shell = pkgs.fish;
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.srid = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "lxd" "ipfs" ]; 
  };

  environment.systemPackages = with pkgs; [
    signal-desktop
    tdesktop
    brave
    # google-chrome -- evil browser makes itself default behind the scenes
    firefox
    pulsemixer
    mpv
    steam
  ];


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

