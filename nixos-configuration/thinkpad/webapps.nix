{ config, lib, pkgs, ... }:

let
  # Dedicated user for running internet-exposed services.
  srvUser = "apps";

  sources = import ../../nix/sources.nix;

  obelisk = import ../../nix/obelisk.nix { inherit config lib pkgs; };
in
{
  # Obelisk apps I expose to the outside world
  systemd.services = {
  };
}
