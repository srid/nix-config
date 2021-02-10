{ pkgs, ... }:

let
  obeliskService = name: port: user: obApp:
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
        Restart = "on-failure";
        PrivateTmp = true;
        User = user;
      };
    };
in
{
  inherit obeliskService; 
}
