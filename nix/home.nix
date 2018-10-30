# https://nixos.wiki/wiki/Home_Manager

{ config, pkgs, ...}:

{
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];
  home-manager.users.srid = {
    programs.git = {
      enable = true;
      userName = "srid";
      userEmail = "srid@srid.ca";
      ignores = [ "*~" "*ghcid.txt" ];
    };
  };
}
