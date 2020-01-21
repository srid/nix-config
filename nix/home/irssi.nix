{ config, lib, pkgs, ... }:

{
   programs.irssi = {
    enable = true;

    networks = {
      freenode = {
        nick = "srid-irssi";
        server = {
          address = "chat.freenode.net";
          port = 6697;
          autoConnect = true;
        };
        channels = {
          nixos.autoJoin = true;
          haskell.autoJoin = true;
          qfpl.autoJoin = true;
          zurihac.autoJoin = true;
          "#rms".autoJoin = true;
          "reflex-frp".autoJoin = true;
        };
      };
    };

    extraConfig = ''
      settings = {
        "fe-common/core" = {
          autolog = "yes";
          autolog_path = "~/keybase/private/srid/irc/$tag/$0.log";
        };
      };
    '';
  };
}
