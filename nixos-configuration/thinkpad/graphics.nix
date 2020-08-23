{ config, lib, pkgs, ... }:

{
  # When using just discrete graphics with nvidia, DPI is calculated
  # automatically,
  # https://http.download.nvidia.com/XFree86/Linux-x86/1.0-8178/README/appendix-y.html

  hardware.video.hidpi.enable = true;

  # services.xserver.
  services.xserver = {
    # Enable touchpad support.
    libinput = {
      enable = true;
      naturalScrolling = true;
    };

    # Graphics drivers.
    videoDrivers = [ "nvidia" "intel" ];

    # Configuration for high res (4k/5k) monitors that use dual channel.
    # Facts:
    #  - TwinView is automatically enabled in recent nvidia drivers (no need to enable it explicitly)
    #  - nvidiaXineramaInfo must be disabled, otherwise X will treat the display as two monitors.
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
  };

  hardware = {
    # TODO: Hybrid graphics.
    #  - Bumblee works, but DPI sucks (no nvidia driver to detect it)
    #  - Connect to external monitor using `optirun true; intel-virtual-output -f` (and arandr)
    #  - Somewhat sluggish performance; monitor 2 duplicates often.
    #  ...
    #  If enabling it back, switch BIOS to hybrid graphics while rebooting.
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

}
