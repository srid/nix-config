{ config, pkgs, ...}:

{
  home.file = {
    ".Xresources".text = ''
      Xft.dpi: 196
    '';
  };
}
