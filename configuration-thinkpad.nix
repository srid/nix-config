# NixOS on Thinkpad P71

# NixOS 17.09 works, but upgrading to nixos-unstable gets you:
#  - Working DPI in Chrome
#  - Trackpoint scroll

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
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "thebeast"; # Define your hostname.

  # Hoping for better graphics performance in latest kernels
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.kernelModules = [
    "nvidia"
    "nvidia_modeset"
    "nvidia_uvm"
    "nvidia_drm"
  ];

  # WiFi
  # Connect to wifi using nmtui / nmcli.
  networking.networkmanager.enable = true;

  services.openssh.enable = true;

  sound.mediaKeys.enable = true;

  # TODO: Hybrid graphics configuration.
  #  - Bumblee works, but DPI sucks (no nvidia driver to detect it)
  #  - Can't connect external monitor via intel card.
  #  - And bumblee can't use nvidia for external monitor.
  # The only solution for laptop use (not branched) for now is to disable hybrid
  # graphics in BIOS and fix the DPI issues manually.
  # hardware.bumblebee = {
  #   enable = false;
  #   driver = "nvidia";
  # };

  # When using just discrete graphics with nvidia, DPI is calculated
  # automatically,
  # https://http.download.nvidia.com/XFree86/Linux-x86/1.0-8178/README/appendix-y.html

  # services.xserver.
  services.xserver = {
    # Enable touchpad support.
    libinput.enable = true;

    # Graphics drivers.
    # I have not figured out how to use hybrid graphics. Intel doesn't connect
    # to an external monitor. To do that,
    #  - enable Discrete graphics in BIOS
    videoDrivers = [  "nvidia" "intel" ];
    # If not using discrete graphics (nvidia)--perhaps to save on
    # battery--power, do the following:
    #  - remove "nvidia" from the list above
    #  - uncomment the DPI line below
    #  - nixos-switch rebuild
    #  - reboot, and switch to hybrid graphics in BIOS.
    #dpi = 200;

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

  nixpkgs.config = {
    allowUnfree = true;

    # Create an alias for the unstable channel
    packageOverrides = pkgs: {
      unstable = import <nixpkgs-unstable> {
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
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
    # Audio
    # Use `pactl set-sink-volume 0 +10%` to increase volume.
    pulseaudio = {
      enable = true;
      support32Bit = true;
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
      # For Bose QC35: https://askubuntu.com/questions/833322
      extraConfig = ''
        [General]
        Disable=Socket
        Disable=Headset
        Enable=Media,Source,Sink,Gateway
        AutoConnect=true
        load-module module-switch-on-connect
        ControllerMode = bredr
        AutoEnable=true
      '';
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
