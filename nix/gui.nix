{ config, pkgs, ...}:

{
  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    windowManager.default = "xmonad";
  };

}
