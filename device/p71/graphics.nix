{ config, lib, pkgs, ... }:

{

  # services.xserver.
  services.xserver = {
    # Graphics drivers.
    videoDrivers = [ "nvidia" "intel" ];

    # THIS COMMENT IS OUT OF DATE
    # Configuration for high res (4k/5k) monitors that use dual channel.
    # Facts:
    #  - TwinView is automatically enabled in recent nvidia drivers (no need to 
    #    enable it explicitly)
    #  - In the past, nvidiaXineramaInfo/Xinerama must be disabled, otherwise X
    #    will treat the display as two monitors. This doesn't have to be done
    #    anymore.
    deviceSection = ''
         Option         "Twinview"   "false"
    '';
  };
}
