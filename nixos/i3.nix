{ config, pkgs, ...}:

# See also: nix/i3-config.nix for dotfiles
let 
  # Suckless Terminal provides good performance. Just need to increase the
  # fontsize on retina display.
  myst = pkgs.writeScriptBin "myst" 
  ''
    #!${pkgs.runtimeShell}
    exec ${pkgs.st}/bin/st -f "monospace:pixelsize=24" $*
  '';
  screenshot = pkgs.writeScriptBin "screenshot"
  '' 
    #!${pkgs.runtimeShell}
    ${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png
  '';
in {
  services.xserver = {
    displayManager = {
      defaultSession = "none+i3";
      # sddm.enable = true;
      sessionCommands = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option "ctrl:nocaps" 

        # Disable turning screen off (cf suspend-crash-workaround.nix)
        xset s off
        xset -dpms
      '';
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
        i3blocks
        arandr
        xorg.xmodmap
        xorg.xdpyinfo
        pulsemixer
        # My scripts
        myst
        screenshot
      ];
    };

    # Auto-lock
    xautolock = {
      enable = true;
      time = 5; # mins

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
