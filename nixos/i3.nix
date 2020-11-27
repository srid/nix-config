{ config, pkgs, ...}:

# See also: nix/i3-config.nix for dotfiles
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
  services.xserver = {
    displayManager = {
      # defaultSession = "none+i3";
      # sddm.enable = true;
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = ./i3.conf;
      extraSessionCommands = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
        Xft.dpi: 192
        EOF
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option "ctrl:nocaps" 

        # Disable turning screen off (cf suspend-crash-workaround.nix)
        xset s off
        xset -dpms
      '';
      extraPackages = with pkgs; [
        dmenu
        i3status
        arandr
        xorg.xmodmap
        xorg.xdpyinfo
        pulsemixer
        # My scripts
        myst
        screenshot
      ];
    };
  };
}
