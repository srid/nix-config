{ config, lib, pkgs, ... }:

{
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
    # Reject everyone but facade and my laptop
    extraCommands = 
      '' 
      # Accept all connections from wireguard peer, facade.
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
      peers = 
        let 
          p = ../../nixos/wireguard/peers; 
        in [
          (p + /facade.nix)
          (p + /x1c7.nix)
        ];
    };
  };

}
