{ pkgs, config,...}:

let 
  apikey = builtins.readFile (../private-config/syncthing/apikey + "/${config.networking.hostName}"); 
in {
  systemd.user.services.syncthing-tray = { 
    enable = true;
    description = "Syncthing Tray";
    wantedBy = [ "graphical-session.target" ];
    environment = {
    };
    serviceConfig = {
      ExecStart = "${pkgs.syncthing-tray}/bin/syncthing-tray -api ${apikey}";
      Restart = "on-abnormal";
    };
  };
}
