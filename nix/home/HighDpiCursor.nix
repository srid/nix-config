{ config, lib, pkgs, ... }:

{
   # Workaround for https://github.com/NixOS/nixpkgs/issues/34603
   xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 128;
  };
}
