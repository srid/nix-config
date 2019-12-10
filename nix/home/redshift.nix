{ config, lib, pkgs, ... }:

{
  # https://en.wikipedia.org/wiki/Redshift_(software)
  services.redshift = {
    enable = true;
    tray = true;
    # Quebec City
    latitude = "46.8423";
    longitude = "-71.2429";
  };
}
