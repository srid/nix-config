{ config, pkgs, ...}:

{
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
      # enough on a retina screen.

      # Issues
      #  - redshift is messing up colors on external monitor.
      enable = false;
    };
  };

  # programs.sway.enable = true;

  environment.systemPackages = with pkgs; [
    dmenu
    i3lock
    maim
    termite
    st
    kitty
    asciinema

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
