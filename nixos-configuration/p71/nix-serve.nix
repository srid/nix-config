{ config, lib, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    5000
  ];

  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
  };
}
