{ config, pkgs, ...}:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+i3";
    };

    windowManager.xmonad = {
      enable = true;
      extraPackages = with pkgs; [
        haskellPackages.xmonad-contrib
      ];
      enableContribAndExtras = true;
    };

    # Auto-lock
    xautolock = {
      enable = true;
      time = 5; # mins

      # Not sure if some modes are the cause of system freeze
      # So deterministically pick one.
      locker = "${pkgs.xlockmore}/bin/xlock -mode space";

      # Suspend after sometime (enable this after things are okay)
      # killtime = 20; # mins
      # killer = "/run/current-system/systemd/bin/systemctl suspend";
    };
  };

  # Monitor plug n play
  # https://github.com/phillipberndt/autorandr/blob/v1.0/README.md#how-to-use
  services.autorandr = {
    enable = true;
  };
}
