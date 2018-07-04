# NixOS on Thinkpad P71

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./nix/base.nix
      ./nix/emacs.nix
      ./nix/gui.nix
      ./nix/dev.nix
      ./myobsidian/myobsidian.nix  # Work configuration (private)
    ];

  # EFI boot
  boot.cleanTmpDir = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.plymouth.enable = true;

  networking.hostName = "thebeast"; # Define your hostname.

  # Hoping for better graphics performance in latest kernels
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # WiFi
  # Connect to wifi using nmtui / nmcli.
  networking.networkmanager.enable = true;

  # Can break during startup. Disabling for now.
  virtualisation.virtualbox.host.enable = false;

  # Want to ssh to thinkpad from macbook
  services.openssh.enable = true;

  sound.mediaKeys.enable = true;

  services.ddclient = {
    enable = true;
    configFile = "/home/srid/.ddclient";
  };

  services.nginx = {
    enable = true;
    virtualHosts."localhost" = {
      locations."/".proxyPass = "http://localhost:9000";
    };
  };


  # When using just discrete graphics with nvidia, DPI is calculated
  # automatically,
  # https://http.download.nvidia.com/XFree86/Linux-x86/1.0-8178/README/appendix-y.html

  # services.xserver.
  services.xserver = {
    # Enable touchpad support.
    libinput.enable = true;

    # Graphics drivers.
    videoDrivers = [ "nvidia" "intel" ];
    # Nvidia detects somehow higher value for a DPI than what is standard for retina. Set it manually:
    # Same as iMac retina 5k https://en.wikipedia.org/wiki/Retina_Display#Models
    dpi = 218;

    # Configuration for high res (4k/5k) monitors that use dual channel.
    # Facts:
    #  - TwinView is automatically enabled in recent nvidia drivers (no need to enable it explicitly)
    #  - nvidiaXineramaInfo must be disabled, otherwise xmonad will treat the display as two monitors.
    screenSection = ''
      Option "nvidiaXineramaInfo"  "false"
    '';

    # FIXME: To work with varied display configurations, I have to use `arandr`.
    # For example, to manual display the laptop screen when connected to
    # external display. This needs to be streamlined.

    # Not sure if this is still required.
    serverFlagsSection = ''
      Option  "Xinerama" "0"
    '';

    displayManager.lightdm = {
      enable = true;
      background = "/home/srid/mynixos/files/Atom-HD-Wallpaper.png";  # FIXME: doesn't work
    };
  };

  environment.systemPackages = with  pkgs; [
    # TODO: Use autorandr to switch between modes.
    # For now, doing it manually using arandr.
    #   Using .screenlayout/lgonly.sh when connecting the monitor
    arandr
    autorandr
    acpi
    # pulsemixer
    # https://github.com/GeorgeFilipkin/pulsemixer#interactive-mode
    # H/L, Shift+Left/Shift+Right   change volume by 10
    # 1/2/3/4/5/6/7/8/9/0           set volume to 10%-100%
    # m                             mute/unmute
    # Mouse wheel                   volume change
    # TODO: Configure xmonad to bring pulsemixer on popup
    # as needed.
    pulsemixer
  ];

  hardware = {
    # TODO: Hybrid graphics.
    #  - Bumblee works, but DPI sucks (no nvidia driver to detect it)
    #  - Connect to external monitor using `optirun true; intel-virtual-output -f` (and arandr)
    #  - Somewhat sluggish performance; monitor 2 duplicates often.
    bumblebee = {
      enable = false;
      driver = "nvidia";
      pmMethod = "none";
      connectDisplay = true;
    };

    # Audio
    # Use `pactl set-sink-volume 0 +10%` to increase volume.
    pulseaudio = {
      enable = true;
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
    };
  };

  # TLP Linux Advanced Power Management
  # Seems to make suspend / wake-up work on lid-close.
  services.tlp.enable = true;

  users.extraUsers.srid = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "audio" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?
}
