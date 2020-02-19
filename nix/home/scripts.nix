{ config, pkgs, ...}:

let 
  mkScript = name: cli: pkgs.writeScriptBin name
  ''
    #!${pkgs.runtimeShell}
    exec ${cli}
  '';
in 
{
  home.packages = with pkgs; [
    (mkScript "lockscreen" 
      "${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force off")
    (mkScript "screenshot" 
      "${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png")

    # Wifi management
    (mkScript "fuckwifi"
      "sh -xe -c 'nmcli radio wifi off; nmcli radio wifi on'")

    # Monitor management
    # Use `arandr` to dump these scripts after fixing layout through GUI.
    (mkScript "monitor-extonly"
      "xrandr --output DP-6 --off --output DP-5 --mode 2560x2880 --pos 0x0 --rotate normal --output DP-4 --off --output DP-3 --mode 2560x2880 --pos 2560x0 --rotate normal --output DP-2 --off --output DP-1 --off --output DP-0 --off")
    (mkScript "monitor-laptoponly"
      "xrandr --output DP-6 --off --output DP-5 --off --output DP-4 --off --output DP-3 --off --output DP-2 --mode 3840x2160 --pos 0x0 --rotate normal --output DP-1 --off --output DP-0 --off")
    (mkScript "monitor-full"
      "xrandr --output DP-6 --off --output DP-5 --primary --mode 2560x2880 --pos 0x0 --rotate normal --output DP-4 --off --output DP-3 --mode 2560x2880 --pos 2560x0 --rotate normal --output DP-2 --mode 3840x2160 --pos 640x2880 --rotate normal --output DP-1 --off --output DP-0 --off")

  ];
}

