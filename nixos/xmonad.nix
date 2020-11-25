{ config, pkgs, ...}:

let 
  # Suckless Terminal provides good performance. Just need to increase the
  # fontsize on retina display.
  myst = pkgs.writeScriptBin "myst" 
  ''
    #!${pkgs.runtimeShell}
    # Use fc-list to lookup font names
    exec ${pkgs.st}/bin/st -f "CascadiaCode:pixelsize=26" $*
  '';
in {
  environment.systemPackages = with pkgs; [
    myst
    dmenu
  ];
  services.xserver = {
    displayManager = {
      defaultSession = "none+xmonad";
    };
    windowManager.xmonad = {
      enable = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
      ];
      enableContribAndExtras = true;
      config = ''
        import XMonad

        main =
          xmonad
            defaultConfig
              { modMask = mod4Mask, -- Use Super instead of Alt
                terminal = "myst"
                -- more changes
              }
        '';
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
