{ config, lib, pkgs, ... }:

{
  services.keybase = {
    enable = true;
  };

  services.kbfs = {
    enable = true;
  };
}
