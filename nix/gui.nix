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
    # brightness.day = 1;
    # brightness.night = 1;
    # New York
    # latitude = "40.7128";
    # longitude = "-74.0060";
  };

  services.compton = {
    enable = true;
    shadow = true;
    inactiveOpacity = "1.0";
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
        unstable.i3status-rust
        # alsaUtils
        speedtest-cli
        gperftools
        # lm-sensors
        compton # for the 'peek' screengrabbing tool
      ];
      extraSessionCommands = ''
        feh --bg-fill ~/mynixos/files/Elephant-Mammoth-Dark.jpg &
        dropbox &
      '';
    };

    windowManager.xmonad = {
      enable = false;
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
  };

  environment.systemPackages = with pkgs; [
    dmenu
    feh
    i3lock
    maim
    peek
    steam
    unstable.alacritty
    unstable.atom
    unstable.dropbox
    unstable.google-chrome-dev
    unstable.vscode
    vlc
    xclip
    xorg.xbacklight
    xorg.xf86videointel
    xsel
    zoom-us
  ];
}
