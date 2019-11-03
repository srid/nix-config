{ config, lib, pkgs, ... }:

{
   services.gpg-agent = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };
}
