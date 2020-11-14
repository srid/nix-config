{ config, pkgs, ...}:

# Based on https://nixos.wiki/wiki/Syncthing
let 
  user = "srid";
  dataDir = "/home/${user}";
in {
  services.syncthing = {
    inherit user dataDir;
    enable = true;
    openDefaultPorts = true;
  };
  environment.systemPackages = [
    pkgs.syncthing-tray
  ];
}
