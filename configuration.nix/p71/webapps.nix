{ config, lib, pkgs, ... }:

let
  sources = import ../../nix/sources.nix;

  obelisk = import ../../nix/obelisk.nix { inherit config lib pkgs; };
in
{
  networking.firewall.allowedTCPPorts = [
    7001 7002 7003 7004 7005 7006
    9990 9991
    80 81
  ];

  # **WARNING** All these services are exposed on the local network.

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;

    virtualHosts = {
      # This will be the default (nginx picks the first) for unrecognized
      # incoming requests.
      "public.srid.ca" = {
        listen = [{ addr = "0.0.0.0"; port = 80; }];
        root = "/var/public";
        locations."/list" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
      "static.srid.ca" = {
        listen = [{ addr = "0.0.0.0"; port = 81; }];
        root = "/var/private";
        basicAuthFile = ../../private-config/machine/godzilla/htpasswd;
        locations."/" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
    };
  };

  # Obelisk apps I expose to the outside world
  systemd.services = {
    slownews =
      obelisk.obeliskService
        "slownews" "7001" (import sources.slownews {});
    MarkdownPreview =
      obelisk.obeliskService
        "MarkdownPreview" "7002" (import sources.MarkdownPreview {});
    MarkdownPreview-wasm =
      obelisk.obeliskService
        "MarkdownPreview-wasm" "7005" (import sources.MarkdownPreview-wasm {});
    slackarchive =
      obelisk.obeliskService
        "slackarchive" "7003" (import sources.Taut {});
    funprog = 
      let 
        # TODO: properly nixify this
        root = "/home/srid/code/zulip-archive";
      in {
        enable = true;
        description = "funprog";
        wantedBy = [ "default.target" ];
        after = [ "network-online.target" ];
        environment = {};
        serviceConfig = {
          # TODO: Properly nixify this
          WorkingDirectory = "${root}";
          ExecStart = "${root}/result/bin/zulip-archive serve -p 7004";
          Restart = "on-abnormal";
          PrivateTmp = true;
          User = "srid";
        };
      };
    github-sponsors = 
      let 
        app = pkgs.callPackage (sources.sponsors-api) {};
      in {
        enable = true;
        wantedBy = [ "default.target" ];
        after = [ "network-online.target" ];
        environment = import ./../../private-config/github-sponsors-env.nix;
        serviceConfig = {
          ExecStart = "${app}/bin/sponsors-api";
          Restart = "on-abnormal";
          PrivateTmp = true;
          User = "apps";
        };
      };
  };
}
