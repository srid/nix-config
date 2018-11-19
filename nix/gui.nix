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
    enable = false;  # Disable for better battery??
    shadow = true;
    inactiveOpacity = "1.0";
  };

  fonts = { 
    enableFontDir = true;
    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-code
      font-awesome-ttf
      google-fonts
      hasklig
      iosevka
      powerline-fonts
      noto-fonts-emoji
      emojione
    ];
  };

  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = true;

    layout = "us";
    xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";

    displayManager.lightdm = {
      enable = true;
      background = "/home/srid/mynixos/files/think.jpg";  # FIXME: doesn't work
    };

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
        feh --bg-fill ~/mynixos/files/think.jpg &
        dropbox &
      '';
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
        haskellPackages.xmobar
      ];
    };
    # windowManager.default = "xmonad";

    desktopManager.plasma5 = {
      # Installing KDE gives me a nice and big cursor that is actually visible
      # enough on a retina screen. And konsole.

      # Issues
      #  - redshift is messing up colors on external monitor.
      enable = false;
    };

    desktopManager.gnome3 = {
      enable = true;
    };
  };

  programs.light.enable = true;

  environment.systemPackages = with pkgs; [
    dmenu
    feh
    i3lock
    maim
    steam

    # To launch peek, C-p peek
    # C+S+<space> to make it floaitng window
    # Postiion window and begin record
    # C+S+4 to move it hidden i3 workspace
    # When recording is done, hit C+4 to bring it back
    # Stop recording.
    peek

    alacritty
    signal-desktop
    google-chrome
    firefox
    vlc
    xclip
    xorg.xbacklight
    xorg.xf86videointel
    xsel
    zoom-us
  ];
}
