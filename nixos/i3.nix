{ config, pkgs, ...}:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+i3";
      # sddm.enable = true;
    };

    libinput.naturalScrolling = true;

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
        i3blocks
        xorg.xmodmap
        arandr
        xorg.xdpyinfo
      ];
    };
  };

  # Monitor plug n play
  # https://github.com/phillipberndt/autorandr/blob/v1.0/README.md#how-to-use
  services.autorandr = {
    enable = true;
  };
}
