{ config, pkgs, ... }:

# LG Ultrafine 5k sometimes produces a vertical glimmering line that only goes away on screen refresh.
# This happens with Nvidia, not intel graphics.
let 
  refreshscreen = pkgs.writeScriptBin "refreshscreen"
  '' 
    #!${pkgs.runtimeShell}
    ${pkgs.xorg.xset}/bin/xset dpms force off
    ${pkgs.xorg.xset}/bin/xset dpms force on
  '';
in {
  environment.systemPackages = [ refreshscreen ];
}
