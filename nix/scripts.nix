{ config, pkgs, ...}:

let 
  mkScript = name: script: pkgs.writeScriptBin name
  ''
    #!${pkgs.runtimeShell}
    ${script}
  '';
  mkAlias = name: cli: mkScript name "exec ${cli}";
in 
{
  home.packages = with pkgs; [
    (mkAlias "lockscreen" 
      "${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force off")
    (mkAlias "screenshot" 
      "${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png")
    (mkScript "screenshot-to-zettelkasten"
      ''
      FILEPATH=static/images/$(date +"%Y-%m-%d-%H-%S.png")
      ${pkgs.maim}/bin/maim -s /home/srid/zettelkasten/$FILEPATH
      echo "![Image]($FILEPATH)" | ${pkgs.xclip}/bin/xclip -selection clipboard -t text/plain
      '')
    # Need this to fix a random nvidia display bug (vertical buzz line)
    (mkScript "refreshscreen"
      "xset dpms force off ; xset dpms force on")

    # Set DISPLAY so xclip will work inside tmux.
    # These won't however work when remotely ssh'ed in (obviously).
    (mkScript "copy"
      "DISPLAY=:0 ${pkgs.xclip}/bin/xclip -f -sel clip")
    (mkScript "paste"
      "DISPLAY=:0 ${pkgs.xclip}/bin/xclip -o -sel clip")

    # Wifi management
    (mkScript "fuckwifi"
    ''
      nmcli radio wifi off
      nmcli radio wifi on
    '')

    # Monitor management
    # Use `arandr` to dump these scripts after fixing layout through GUI.
    (mkAlias "monitor-extonly"
      "xrandr --output DP-6 --off --output DP-5 --mode 2560x2880 --pos 0x0 --rotate normal --output DP-4 --off --output DP-3 --mode 2560x2880 --pos 2560x0 --rotate normal --output DP-2 --off --output DP-1 --off --output DP-0 --off")
    (mkAlias "monitor-laptoponly"
      "xrandr --output DP-6 --off --output DP-5 --off --output DP-4 --off --output DP-3 --off --output DP-2 --mode 3840x2160 --pos 0x0 --rotate normal --output DP-1 --off --output DP-0 --off")
    (mkAlias "monitor-full"
      "xrandr --output DP-6 --off --output DP-5 --primary --mode 2560x2880 --pos 0x0 --rotate normal --output DP-4 --off --output DP-3 --mode 2560x2880 --pos 2560x0 --rotate normal --output DP-2 --mode 3840x2160 --pos 640x2880 --rotate normal --output DP-1 --off --output DP-0 --off")

  ];
}

