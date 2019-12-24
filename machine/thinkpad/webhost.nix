{ config, lib, pkgs, ... }:

let
  obeliskService = name: port: repo: rev:
    let
      obresult = (import (builtins.fetchTarball "https://github.com/${repo}/archive/${rev}.tar.gz") {}).exe;
      root = pkgs.runCommand "${name}-service" {}
        ''
        mkdir $out
        cp -r ${obresult}/* $out/
        cp -r ${./deployments}/${name}/config $out/
        '';
    in import ../../nix/obelisk-app.nix {
      inherit pkgs name port root;
    };
in
{
  # How I use my Thinkpad as a web server on the internet

  # First, dedicate a user for running internet-exposed services.
  users.extraUsers.apps = {
    isNormalUser = true;
    uid = 1001;
  };

  # Obelisk apps I expose outside from the Thinkpad.
  systemd.services = {
    slownews = obeliskService "slownews" "3001" "srid/slownews" "badc5fd";
    # slackarchive = obeliskService "SlackArchive" "9002" "srid/Taut" "...";  -- TODO: config
  };
}
