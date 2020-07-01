{ config, lib, pkgs, ... }:

let 
  mod = "Mod4";
in {
  xsession.scriptPath = ".hm-xsession"; # Ref: https://discourse.nixos.org/t/opening-i3-from-home-manager-automatically/4849/8

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;

      fonts = ["DejaVu Sans Mono, FontAwesome 6"];

      keybindings = lib.mkOptionDefault {
        "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+x" = "exec screenshot";
        "${mod}+Ctrl+x" = "exec screenshot-to-zettelkasten";
        "${mod}+Shift+x" = "exec lockscreen";

        # Focus
        "${mod}+j" = "focus left";
        "${mod}+k" = "focus down";
        "${mod}+l" = "focus up";
        "${mod}+semicolon" = "focus right";

        # Move
        "${mod}+Shift+j" = "move left";
        "${mod}+Shift+k" = "move down";
        "${mod}+Shift+l" = "move up";
        "${mod}+Shift+semicolon" = "move right";

        # My multi monitor setup
        "${mod}+m" = "move workspace to output DP-2";
        "${mod}+Shift+m" = "move workspace to output DP-5";

        # Quick floating windows
        "${mod}+Shift+p" = "exec myst -c floating -g=150x50 tmux new-session -s floating ~/code/self/bin/open-zettel";

        # Swap
        "${mod}+i" = "mark _last; focus left; swap with mark \"_last\" ";
        "${mod}+Shift+i" = "mark _last; focus right; swap with mark \"_last\" ";
      };

      bars = [
        {
          position = "bottom";
          statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${./i3status-rust.toml}";
        }
      ];
    };
    extraConfig = ''
      for_window [class="floating"] floating enable;
    '';
  };
}
