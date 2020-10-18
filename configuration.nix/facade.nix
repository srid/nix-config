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
        ../nixos/wireguard/peers/p71.nix
        ../nixos/wireguard/peers/x1c7.nix
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
    in {
      enable = true;
      recommendedProxySettings = true;

      # Private stuff 
      virtualHosts."static.srid.ca" = myVhost {
        # Inner nginx uses htpasswd
        port = 81;
      };
      virtualHosts."zk.srid.ca" = myVhost {
        port = 9000;
        basicAuthFile = ../private-config/machine/godzilla/htpasswd;
      };

      # To Nginx
      virtualHosts."public.srid.ca" = myVhost { port = 80; };


      # Web apps

      virtualHosts."slownews.srid.ca" = myVhost { port = 7001; };
      virtualHosts."commonmark.srid.ca" = myVhost { port = 7002; };
      virtualHosts."slackarchive.actualists.org" = myVhost { 
        port = 7003; 
        basicAuthFile = ../private-config/machine/godzilla/slackarchive.htpasswd;
      };
      virtualHosts."funprog.srid.ca" = myVhost { port = 7004; };
      virtualHosts."commonmark-wasm.srid.ca" = myVhost { port = 7005; };

      virtualHosts."github-sponsors.srid.ca" = myVhost { port = 7006; };

      virtualHosts."cache.srid.ca" = myVhost { 
        port = 5000; 
        basicAuthFile = ../private-config/binary-cache/htpasswd;
      };
    };

  security.acme = {
    acceptTerms = true;
    email = "srid@srid.ca";
  };

  system.stateVersion = "20.03";
}
