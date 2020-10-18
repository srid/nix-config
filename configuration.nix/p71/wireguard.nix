{ config, lib, pkgs, ... }:

{
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
    # Accept all connections from wireguard peer, facade.
    extraCommands = 
      '' 
      iptables -I INPUT -p tcp -s 10.100.0.1 -j ACCEPT
      # From X1C7 as well
      iptables -I INPUT -p tcp -s 10.100.0.3 -j ACCEPT
      '';
  };

  # Wireguard client
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.2/24" ];
      listenPort = 51820;
      privateKeyFile = "/home/srid/nix-config/private-config/wireguard/bornagain/private";
      peers = [
        ../../nixos/wireguard/peers/facade.nix
        ../../nixos/wireguard/peers/x1c7.nix
      ];
    };
  };

}
