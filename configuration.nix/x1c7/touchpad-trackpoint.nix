{ config, pkgs, ... }:

{
   services.xserver.libinput = {
      enable = true;
      # macOS like behaviour
      naturalScrolling = true;
      # Tap to click
      tapping = true;
    };
}
