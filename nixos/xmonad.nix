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
  screenshot = pkgs.writeScriptBin "screenshot"
  '' 
    #!${pkgs.runtimeShell}
    ${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png
  '';
in {
  environment.systemPackages = with pkgs; [
    myst
    screenshot
    dmenu
    gmrun
    xmobar
    dzen2
  ];
  services.xserver = {
    enable = true;
    displayManager = {
      defaultSession = "none+xmonad";
    };
    windowManager.xmonad = {
      enable = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
      ];
      enableContribAndExtras = true;
      config = pkgs.lib.readFile ./xmonad-srid/Main.hs;
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
