{ config, pkgs, ...}:

# See also: nixos/i3.nix for global configuration
{
  home.file = {
    ".Xresources".text = ''
      Xft.dpi: 196
    '';
  };
}
