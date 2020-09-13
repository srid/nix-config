{ config, pkgs, ...}:

{
  services.syncthing = {
    enable = true;
    tray = true;
  };
}
