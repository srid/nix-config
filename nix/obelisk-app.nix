{ pkgs, root, name, port, ... }:

let
  app = import root {};
in
{
  enable = true;
  description = name;
  wantedBy = [ "multi-user.target" ];
  after = [ "network-online.target" ];
  environment = {
  };
  serviceConfig = {
    # TODO: copy over files, and allow overriding ./config dir.
    WorkingDirectory = "${app.exe}";
    ExecStart = "${app.exe}/backend -p ${port}";
    ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
    Restart = "on-failure";
    PrivateTmp = true;
    User = "apps";
  };
}
