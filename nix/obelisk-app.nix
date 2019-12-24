{ pkgs, root, name, port, ... }:

{
  enable = true;
  description = name;
  wantedBy = [ "multi-user.target" ];
  after = [ "network-online.target" ];
  environment = {
  };
  serviceConfig = {
    WorkingDirectory = "${root}";
    ExecStart = "${root}/backend -p ${port}";
    ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
    Restart = "on-failure";
    PrivateTmp = true;
    User = "apps";
  };
}
