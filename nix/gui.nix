{ config, pkgs, ...}:

{
  # I depend on Google Chrome
  nixpkgs.config.allowUnfree = true;

  # Google Chrome ulimit upping
  # https://bugs.chromium.org/p/chromium/issues/detail?id=362603#c28
  security.pam.loginLimits = [
    { domain = "*"; item = "nofile"; type = "-"; value = "65536"; }
  ];

  services.redshift = {
    enable = true;
    # Quebec City
    latitude = "46.8423";
    longitude = "-71.2429";
    # temperature.day = 5500
    # temperature.night = 3700
    # brightness.day = 1
    # brightness.night = 1
    # New York
    # latitude = "40.7128";
    # longitude = "-74.0060";
  };

  programs.sway = {
    enable = true;
  };

  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = true;

    layout = "us";
    xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        i3status-rust
        alsaUtils
        speedtest-cli
        gperftools
        # lm-sensors
      ];
      extraSessionCommands = ''
        feh --bg-fill ~/mynixos/files/Elephant-Mammoth-Dark.jpg &
        dropbox &
      '';
    };

    windowManager.dwm = {
      enable = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    # windowManager.default = "xmonad";

    desktopManager.plasma5 = {
      # Installing KDE gives me a nice and big cursor that is actually visible
      # enough on a retina screen.

      # Issues
      #  - redshift is messing up colors on external monitor.
      enable = true;
    };

    desktopManager.gnome3 = {
      # XXX: Signing into a Gnome session only gives a black screen.
      enable = true;
    };
  };


  environment.systemPackages = with pkgs; [
    steam

    redshift-plasma-applet

    unstable.alacritty
    unstable.dropbox
    # unstable.dropbox-cli
    unstable.google-chrome
    # konsole
    unstable.firefox
    unstable.anki
    rxvt_unicode

    signal-desktop
    unstable.vscode
    atom

    # Notification
    dunst 
    libnotify

    # X utilities
    dmenu
    dzen2
    haskellPackages.xmobar
    slock
    maim
    xclip
    i3lock
    feh
    imagemagick
    xorg.xbacklight

    zoom-us
  ];

  # slock needs OOM exception
  # https://github.com/NixOS/nixpkgs/issues/9656#issuecomment-137719381
  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";
}
