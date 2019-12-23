{ pkgs, approot, port, ... }:

let
  app = import approot {};
in
{
  enable = true;
  description = "SlowNews";
  wantedBy = [ "multi-user.target" ];
  after = [ "network-online.target" ];
  environment = {
  };
  # TODO: Run as different user?
  serviceConfig = {
    # TODO: copy over files, and allow overriding ./config dir.
    WorkingDirectory = "${app.exe}";
    ExecStart = "${app.exe}/backend -p ${port}";
    ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
    Restart = "on-failure";
    PrivateTmp = true;
  };
}
