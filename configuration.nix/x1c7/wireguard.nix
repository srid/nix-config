{ config, pkgs, ... }:

{
  # Set up Wireguard
  networking = {
    firewall = {
      enable = true;
      allowedUDPPorts = [51820];
    };
    wireguard.interfaces = {
      wg0 = {
        ips = [ "10.100.0.3/24" ];
        listenPort = 51820;
        privateKeyFile = "/home/srid/nix-config/private-config/wireguard/x1c7/private";
        peers = [
          ../../nixos/wireguard/peers/facade.nix
          ../../nixos/wireguard/peers/p71.nix
        ];
      };
    };
  };

}
