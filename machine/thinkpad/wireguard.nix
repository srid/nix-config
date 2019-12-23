{ config, lib, pkgs, ... }:

{
  # Wireguard client
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.2/24" ];
      listenPort = 51820;
      privateKeyFile = "/home/srid/wireguard-keys/private";
      peers = [
        { publicKey = "8BLMztljWIV+9V+fXgj34GCVn0YSK6PYdAuPVkdidTs=";
          allowedIPs = [ "10.100.0.1" "104.198.14.52" ];
          endpoint = "facade.srid.ca:51820";
          persistentKeepalive = 25;
        }
        # pixel slate
        { publicKey = "yMuIxno/f/eI5W+P6SsBZ0Ib5s0uhqEo/DB8MdCbryY=";
          allowedIPs = [ "10.100.0.3" ];
          persistentKeepalive = 25;
        }
      ];
    };
  };

}
