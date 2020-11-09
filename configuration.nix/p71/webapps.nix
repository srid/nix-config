{ config, lib, pkgs, ... }:

let
  sources = import ../../nix/sources.nix;

  obelisk = import ../../nix/obelisk.nix { inherit config lib pkgs; };
in
{
  # **NOTE** These are not exposed to local network; only wireguard peer (see wireguard.nix)

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
        # - configure API keys
        # - specify output dir as cli param
        # - deal with ./static dir (user_uploads)
        root = "/home/srid/code/zulip-archive";
      in {
        enable = true;
        description = "funprog";
        wantedBy = [ "default.target" ];
        after = [ "network-online.target" ];
        environment = {};
        serviceConfig = {
          WorkingDirectory = "${root}";
          ExecStart = "${root}/result/bin/zulip-archive -w";
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
