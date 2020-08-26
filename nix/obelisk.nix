{ pkgs, ... }:

let
  # Dedicated user for running internet-exposed services.
  srvUser = "apps";

  obeliskService = name: port: obApp:
    let
      obAppWithConfig = pkgs.runCommand "${name}-service" {}
        ''
        mkdir $out
        cp -r ${obApp.exe}/* $out/
        # Copy deployment config from a private area.
        cp -r ${../private-config/deployments}/${name}/config $out/
        '';
    in {
      enable = true;
      description = name;
      wantedBy = [ "default.target" ];
      after = [ "network-online.target" ];
      environment = {};
      serviceConfig = {
        WorkingDirectory = "${obAppWithConfig}";
        ExecStart = "${obAppWithConfig}/backend -p ${port}";
        Restart = "on-abnormal";
        PrivateTmp = true;
        User = srvUser;
      };
    };
in
{
  inherit obeliskService; 
}
