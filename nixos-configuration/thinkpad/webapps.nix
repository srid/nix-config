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
    /* slownews =
      obelisk.obeliskService 
        "slownews" "7001" (import sources.slownews {}); */
    MarkdownPreview =
      obelisk.obeliskService
        "MarkdownPreview" "7002" (import sources.MarkdownPreview {});
  };
}
