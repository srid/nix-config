{ config, pkgs, ... }:

{
   services.xserver.displayManager.sessionCommands = ''
   ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option "ctrl:nocaps" 
   '';
}
