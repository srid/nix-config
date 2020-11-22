{ config, pkgs, ... }:

{
   services.xserver.libinput = {
      enable = true;
      # macOS like behaviour
      naturalScrolling = true;
      # Increase touchpad/trackpoint speed. 1.0 is maximum speed.
      # Changing this value won't take effect until X restart.
      accelSpeed = "0.5";
    };

}
