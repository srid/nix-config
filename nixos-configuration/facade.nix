# Made for my server.

let 
  hostName = "facade";
in
{ config, pkgs, ... }: {
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/networking.nix
    <home-manager/nixos>
  ];
  home-manager.users.srid = (import ../nix/home.nix { 
    inherit pkgs config hostName;
  } );

  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  users.extraUsers = {
    srid = {
      isNormalUser = true;
      extraGroups = [ "wheel" "lxd" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [ (builtins.readFile ../private-config/ssh/id_rsa.pub) ];
    };
  };

  environment.systemPackages = with pkgs; [
    wireguard
    apacheHttpd # For htpasswd
  ];

  networking = { inherit hostName; };

  networking.firewall.allowPing = true;
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
        inherit basicAuthFile;
        enableACME = withSSL;
        forceSSL = withSSL;
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
      recommendedProxySettings = true;

      # Redirects
      virtualHosts."notes.srid.ca" = notesVhost;

      # Private stuff 

      virtualHosts."static.srid.ca" = myVhost {
        port = 4444;
        basicAuthFile = ../private-config/machine/godzilla/htpasswd;
      };

      # Web apps

      virtualHosts."slownews.srid.ca" = myVhost { port = 7001; };
      virtualHosts."commonmark.srid.ca" = myVhost { port = 7002; };
      virtualHosts."slackarchive.actualists.org" = myVhost { 
        port = 7003; 
        basicAuthFile = ../private-config/machine/godzilla/slackarchive.htpasswd;
      };
      virtualHosts."funprog.srid.ca" = myVhost { port = 7004; };
      virtualHosts."staging.cerveau.app" = myVhost { port = 7005; };

      # Multi site: https://tmp.srid.ca/p/9990 => bornagain:9990
      virtualHosts."tmp.srid.ca" = myVhostPortRange { prefix = "p/(999[0-9])"; };
    };

  security.acme = {
    acceptTerms = true;
    email = "srid@srid.ca";
  };

  system.stateVersion = "20.03";
}
