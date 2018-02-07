{ config, pkgs, ...}:

{
  # I depend on Google Chrome
  nixpkgs.config.allowUnfree = true;

  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = true;

    layout = "us";
    xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmobar
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    # windowManager.default = "xmonad";
  };

  environment.systemPackages = with pkgs; [
    alacritty
    dmenu
    dropbox-cli
    google-chrome
    konsole
    rxvt_unicode

    signal-desktop
    vscode

    # X utilities
    slock
    maim
    xclip
    i3lock
    feh
    imagemagick
    xorg.xbacklight
  ];

  # slock needs OOM exception
  # https://github.com/NixOS/nixpkgs/issues/9656#issuecomment-137719381
  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";
}
