{ config, pkgs, ...}:

# See also: nixos/i3.nix for global configuration
{
  # High DPI cursor fix
  # https://github.com/NixOS/nixpkgs/issues/34603
  xsession.pointerCursor = {
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    size = 128;
  };
}
