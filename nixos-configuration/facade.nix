# Made for my server.

let 
  nixosKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYQ003p7fB5ICQehLwhDBomY9WzkNBeijkSw9ADGU+ECrPakeIH3pntUWRJH1W93vKnLqpkn6HLGEXD9MCR0s98uhh8hT7uAYCxQTbEeKT3PYkfz3oe7XaR8rE601sds0ZyFwH7l8cvK97pGr+uhFXAaohiV6VqmLVXhManEjZZ8GfYWBD9BCmIJk43G3OGa5QYFeHqztprXaJNU5dFPv2Uq2C+L6EvfCfkK2OO1BLZgL+Rai5jjyy6k0fcfsxxd9BdGUwqDhcBeyTIzX9rePMugf/xD+6uNRxTU+vjVpGUtFOw6rpgmVyFv9mn3QMNdQBc5hYKVbIQwMNGTzGgcQv srid@nixos";
in
{ config, pkgs, ... }: {
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/networking.nix
    <home-manager/nixos>
  ];
  home-manager.users.srid = (import ../nix/home.nix { 
    inherit pkgs config;
    device = "facade"; 
  } );

  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  networking.firewall.allowPing = true;

  users.extraUsers = {
    srid = {
      isNormalUser = true;
      extraGroups = [ "wheel" "lxd" ];
      # TODO: replace with keyFiles (see bare.nix)
      openssh.authorizedKeys.keys = [
        nixosKey
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    wireguard
    apacheHttpd # For htpasswd
  ];

  networking.hostName = "facade";

  # Wireguard: https://nixos.wiki/wiki/Wireguard
  networking.nat.enable = true;
  networking.nat.externalInterface = "eth0";
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
    allowedTCPPorts = [ 22 1984 80 443 ];
  };
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.1/24" ];
      listenPort = 51820;
      privateKeyFile = "/home/srid/nix-config/private-config/wireguard/facade/private";
      peers = [
        # bornagain
        { publicKey = "z298c6R+NXecUAEQmw/vdNMnewOT6nZ7Tx5q4Kh+5z0=";
          allowedIPs = [ "10.100.0.2/32" ];
        }
        # pixel slate
        #{ publicKey = "yMuIxno/f/eI5W+P6SsBZ0Ib5s0uhqEo/DB8MdCbryY=";
        #  allowedIPs = [ "10.100.0.3/32" ];
        #}
      ];
    };
  };

  services.openssh = {
    enable = true;
    ports = [1984];
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  services.do-agent.enable = true;

  # My apps
  services.nginx = 
    let 
      myVhost = 
      { port
      , withSSL ? true
      , location ? "/"
      , locationExtraConfig ? "proxy_pass http://10.100.0.2:${toString port};" 
      , basicAuthFile ? null
      }: {
        enableACME = withSSL;
        forceSSL = withSSL;
        basicAuthFile = basicAuthFile;
        locations.${location} = {
          proxyWebsockets = true;
          extraConfig = locationExtraConfig;
        };
      };
      # Set up a sub domain that points a range of ports, mapped from sub path.
      myVhostPortRange = { prefix, withSSL ? true }: myVhost { 
        port = 0; 
        withSSL = withSSL; 
        location = "~ /${prefix}/"; 
        locationExtraConfig = ''
          rewrite ^/${prefix}/(.*)$ /$2 break;
          proxy_pass http://10.100.0.2:$1;
        '' ;
      };
      # A vhost that simply redirects old notes to my new site
      notesVhost = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          extraConfig = ''
            rewrite ^/haskell-nix$ https://www.srid.ca/haskell-nix.html permanent;
            rewrite ^/computing/haskell-nix$ https://www.srid.ca/haskell-nix.html permanent;
            rewrite ^/calisthenics$ https://www.srid.ca/calisthenics.html permanent;
            rewrite ^/carnivore-diet$ https://www.srid.ca/carnivore-diet.html permanent;
            rewrite ^/conflicts$ https://www.srid.ca/conflicts.html permanent;
            rewrite ^/lasik$ https://www.srid.ca/lasik.html permanent;
            rewrite ^/sous-vide$ https://www.srid.ca/sous-vide.html permanent;
            rewrite ^/$ https://www.srid.ca/ permanent;
          '' ;
        };
      };
    in {
      enable = true;

      # Redirects
      virtualHosts."notes.srid.ca" = notesVhost;

      # Web apps

      virtualHosts."slownews.srid.ca" = myVhost { port = 7001; };
      virtualHosts."commonmark.srid.ca" = myVhost { port = 7002; };
      virtualHosts."slackarchive.actualists.org" = myVhost { port = 7003; };
      virtualHosts."funprog.srid.ca" = myVhost { port = 7004; };
      # Multi site: https://tmp.srid.ca/p/9990 => bornagain:9990
      virtualHosts."tmp.srid.ca" = myVhostPortRange { prefix = "p/(999[0-9])"; };
    };

  security.acme = {
    acceptTerms = true;
    email = "srid@srid.ca";
  };

  system.stateVersion = "20.03";
}
