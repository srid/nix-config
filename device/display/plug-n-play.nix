{ config, pkgs, ... }:

{
  # Monitor plug n play
  # https://github.com/phillipberndt/autorandr/blob/v1.0/README.md#how-to-use
  services.autorandr = {
    enable = true;
  };

}