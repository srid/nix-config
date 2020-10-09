{ config, lib, pkgs, ... }:

{
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
    #  - TwinView is automatically enabled in recent nvidia drivers (no need to 
    #    enable it explicitly)
    #  - In the past, nvidiaXineramaInfo/Xinerama must be disabled, otherwise X
    #    will treat the display as two monitors. This doesn't have to be done
    #    anymore.
  };

  hardware = {
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
