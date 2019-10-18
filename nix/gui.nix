{ config, pkgs, ...}:

{
  # I depend on Google Chrome
  nixpkgs.config = {
    allowUnfree = true;
  };

  # Google Chrome ulimit upping
  # https://bugs.chromium.org/p/chromium/issues/detail?id=362603#c28
  security.pam.loginLimits = [
    { domain = "*"; item = "nofile"; type = "-"; value = "65536"; }
  ];

  services.compton = {
    enable = true;
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

  services.xserver = {
    enable = true;

    layout = "us";
    xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";

    displayManager.lightdm = {
      enable = true;
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        i3status-rust
        # alsaUtils
        speedtest-cli
        gperftools
        # lm-sensors
        compton # for the 'peek' screengrabbing tool
      ];
      extraSessionCommands = ''
        # feh --bg-fill ~/mynixos/files/think.jpg &
        dropbox &
      '';
    };

    # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
    windowManager.xmonad = {
      enable = false;
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
      # For gnome-terminal
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    dmenu
    feh
    i3lock
    maim
    unstable.steam

    # To launch peek, C-p peek
    # C+S+<space> to make it floaitng window
    # Postiion window and begin record
    # C+S+4 to move it hidden i3 workspace
    # When recording is done, hit C+4 to bring it back
    # Stop recording.
    peek
    simplescreenrecorder

    vscode
    signal-desktop
    google-chrome
    xclip
    xorg.xbacklight
    xorg.xf86videointel
    xorg.xprop
    xsel
    zoom-us
  ];
}
