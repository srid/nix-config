{ config, pkgs, ...}:

with {
  homeManager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/master.tar.gz";
};

{
  imports = [
    "${homeManager}/nixos"
    # ./home/tmux.nix
  ];
  home-manager.users.srid = (import ./home.nix);
}
