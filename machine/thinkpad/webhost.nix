{ config, lib, pkgs, ... }:

{
  # How I use my Thinkpad as a web server on the internet

  # First, dedicate a user for running internet-exposed services.
  users.extraUsers.apps = {
    isNormalUser = true;
    uid = 1001;
  };

  # Obelisk apps I expose outside from the Thinkpad.
  systemd.services = {
    slownews = import ../../nix/obelisk-app.nix {
      inherit pkgs;
      name = "SlowNews";
      root =
        let rev = "badc5fd";
        in builtins.fetchTarball "https://github.com/srid/slownews/archive/${rev}.tar.gz";
      port = "3001";
    };
  };

}
