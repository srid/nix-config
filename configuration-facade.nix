# Made for my server.

{ config, pkgs, ... }: {
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/nixos-in-place.nix
    ./nix/base.nix
    ./nix/srid-home.nix
    # ./nix/dev.nix
    # ./myobsidian/nixos-configuration/cache.nix
  ];

  boot.cleanTmpDir = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  users.extraUsers.srid = {
    isNormalUser = true;
    extraGroups = [ "wheel" "lxd" ];
    openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDDxb8ZoHT4EYdGjSslIUMSFrsoRh/4cdRJXBgS0878Kv/rDRR+f33bh9Hunmx0m78g5bG3/b6C4AMmfcqgw7XvT6yuW0NGjKQeOQtCX6FSu5F+cEv63r7FSjAXEQ6FkJHaFELG2f1wIU43mCVTutAQsiLy0a7NaH7EyxCk1OUXN4FByd2slqGPeLfDEjNQLGiZaYrG4VEfkl1jlgSHWK9ryiahp9IuR4mOTtwRf7fl4DoCAKpEY5jGNZJTe2HubzMAjtxSVcR5KWd7kJYVLw3SsA3NC8o8k9K0rFj2WDKHst0dpBfYjPTYnWZAu3hytrTxS/IB87XUtFjBwQhQk59b srid@MacBook-Pro-de-Sridhar.local"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYQ003p7fB5ICQehLwhDBomY9WzkNBeijkSw9ADGU+ECrPakeIH3pntUWRJH1W93vKnLqpkn6HLGEXD9MCR0s98uhh8hT7uAYCxQTbEeKT3PYkfz3oe7XaR8rE601sds0ZyFwH7l8cvK97pGr+uhFXAaohiV6VqmLVXhManEjZZ8GfYWBD9BCmIJk43G3OGa5QYFeHqztprXaJNU5dFPv2Uq2C+L6EvfCfkK2OO1BLZgL+Rai5jjyy6k0fcfsxxd9BdGUwqDhcBeyTIzX9rePMugf/xD+6uNRxTU+vjVpGUtFOw6rpgmVyFv9mn3QMNdQBc5hYKVbIQwMNGTzGgcQv srid@nixos"
    ];
  };

  environment.systemPackages = with pkgs; [
    wireguard
  ];

  networking.hostName = "facade";
  networking.nat = {
    enable = true;
    externalInterface = "ens3";  # on DigitalOcean as of Dec '18
    internalInterfaces = [ "wg0" ];
  };
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.1/24" ];
      listenPort = 51820;
      privateKeyFile = "/home/srid/wireguard-keys/private";
      peers = [
        # thebeast
        { publicKey = "RNq+9jWeHGZuYUw41abU9hC8ldU03Y7tKrhbvhrwj3A=";
          allowedIPs = [ "10.100.0.2/32" ];
        }
      ];
    };
  };

  services.openssh = {
    enable = true;
    ports = [22];
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  # My apps
  services.nginx = 
    let myVhost = { port, withSSL ? true }: {
      enableACME = withSSL;
      forceSSL = withSSL;
      locations."/" = {
        proxyPass = "http://10.100.0.2:" + toString port;
        proxyWebsockets = true;
      };
    };
    in {
      enable = true;
      user = "srid";
      virtualHosts."irc.srid.ca" = myVhost { port = 9000; };
      virtualHosts."slownews.srid.ca" = myVhost { port = 9001; };
      virtualHosts."riceneggs.srid.ca" = myVhost { port = 9002; };
      virtualHosts."tmp.srid.ca" = myVhost { port = 9999; withSSL = false; };
    };

  security.acme.certs = {
    "slownews.srid.ca".email = "srid@srid.ca";
    "riceneggs.srid.ca".email = "srid@srid.ca";
    "irc.srid.ca".email = "srid@srid.ca";
  };
}
