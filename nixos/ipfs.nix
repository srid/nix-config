{ config, pkgs, ...}:

{
  services.ipfs = {
    enable = true;
    user = "srid";
    autoMount = true;
    # Expose, for wireguard
    gatewayAddress = "/ip4/0.0.0.0/tcp/8080";
  };
}
