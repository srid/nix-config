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
    ];

  # EFI boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "thebeast"; # Define your hostname.

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

  # services.xserver.
  services.xserver = {
    # Enable touchpad support.
    libinput.enable = true;

    # Nvidia
    # Note: discrete graphics must be enabled in bios.
    videoDrivers = [ "nvidia" "intel" ];

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

    # If docked with my Lg Ultrafine 5k, and not using the laptop screen, I can
    # hardcode the following configuration:
    #  Option "MetaModes"           "DP-5: 2560x2880, DP-3: 2560x2880"
    #  Option "ConnectedMonitor"    "DP-5, DP-3"
    #  Option "MetaModeOrientation" "DP-3 RightOf DP-5"

    # Not sure if this is still required.
    serverFlagsSection = ''
      Option  "Xinerama" "0"
    '';
  };

  environment.systemPackages = with  pkgs; [
    # TODO: Use autorandr to switch between modes.
    # For now, doing it manually using arandr.
    #   Using .screenlayout/lgonly.sh when connecting the monitor
    arandr
    autorandr

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

  services.xserver.displayManager.lightdm.enable = true;

  # Audio
  # Use `pactl set-sink-volume 0 +10%` to increase volume.
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

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
