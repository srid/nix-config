{ config, pkgs, ... }:

# https://www.srid.ca/45997a20.html
{
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    IdleAction=suspend
    IdleActionSec=40m
  '';
  systemd.targets.sleep.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.hybrid-sleep.enable = false;
}
