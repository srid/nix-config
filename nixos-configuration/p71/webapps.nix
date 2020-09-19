{ config, lib, pkgs, ... }:

let
  sources = import ../../nix/sources.nix;

  obelisk = import ../../nix/obelisk.nix { inherit config lib pkgs; };
in
{
  networking.firewall.allowedTCPPorts = [
    7001 7002 7003 7004 7005
  ];

  # Obelisk apps I expose to the outside world
  systemd.services = {
    slownews =
      obelisk.obeliskService
        "slownews" "7001" (import sources.slownews {});
    MarkdownPreview =
      obelisk.obeliskService
        "MarkdownPreview" "7002" (import sources.MarkdownPreview {});
    slackarchive =
      obelisk.obeliskService
        "slackarchive" "7003" (import sources.Taut {});
    cerveau-www-staging = 
      obelisk.obeliskService 
        "cerveau-www-staging" "7005" (import sources.cerveau {}); 
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
  };
}
