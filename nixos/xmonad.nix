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

  # TODO: abstract this
  launch-hack = pkgs.writeScriptBin "launch-hack" 
  ''
    #!${pkgs.runtimeShell}
    source ${../bin/launch-hack.sh}
  '';
  launch-zk = pkgs.writeScriptBin "launch-zk" 
  ''
    #!${pkgs.runtimeShell}
    source ${../bin/launch-zk.sh}
  '';
in {
  environment.systemPackages = with pkgs; [
    myst
    screenshot
    launch-hack
    launch-zk

    dmenu
    gmrun
    xmobar
    dzen2
    # For taffybar?
    hicolor-icon-theme
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

  };
}
