{ config, pkgs, ...}:

{
  services.ipfs = {
    enable = true;
    user = "srid";
    autoMount = true;
  };
}
