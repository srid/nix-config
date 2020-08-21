{ config, lib, pkgs, ... }:

let
  # Dedicated user for running internet-exposed services.
  srvUser = "apps";

  # TODO: drop in favour of niv
  publicRepo = repo: rev: sha256:
    import (pkgs.fetchFromGitHub {
      inherit repo rev sha256;
      owner = "srid";
    }) {};

  obeliskService = name: port: r:
    let
      root = pkgs.runCommand "${name}-service" {}
        ''
        mkdir $out
        cp -r ${r.exe}/* $out/
        # Copy deployment config from a private area.
        cp -r ${../private-config/deployments}/${name}/config $out/
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
  inherit publicRepo obeliskService; 
}
