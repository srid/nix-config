# How I use my Thinkpad as a web server on the internet
{ config, lib, pkgs, ... }:

let
  # Dedicated user for running internet-exposed services.
  srvUser = "apps";

  obeliskService = name: port: repo: rev:
    let
      obresult = (import (builtins.fetchTarball "https://github.com/${repo}/archive/${rev}.tar.gz") {}).exe;
      root = pkgs.runCommand "${name}-service" {}
        ''
        mkdir $out
        cp -r ${obresult}/* $out/
        # Copy deployment config from a private area.
        cp -r ${../../private-config/deployments}/${name}/config $out/
        '';
    in {
      enable = true;
      description = name;
      wantedBy = [ ];
      after = [ "network-online.target" ];
      environment = {};
      serviceConfig = {
        WorkingDirectory = "${root}";
        ExecStart = "${root}/backend -p ${port}";
        Restart = "on-abnormal";
        PrivateTmp = true;
        User = srvUser;
      };
    };
in
{
  # Obelisk apps I expose to the outside world
  systemd.services = {
    slownews = obeliskService "slownews" "3001" "srid/slownews" "909f6ea0d83ed3f95177d6c8a3f70ffc5ad175b7";
    MarkdownPreview = obeliskService "MarkdownPreview" "3002" "srid/MarkdownPreview" "89ec7c2fb2206f01219709f86c29fd53f12e8218";
    slackarchive = obeliskService "slackarchive" "9002" "srid/Taut" "ef07c2c";
  };

  users.extraUsers.${srvUser} = {
    isNormalUser = true;
    uid = 1001;
  };
}
