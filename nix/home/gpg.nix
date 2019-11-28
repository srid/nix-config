{ config, lib, pkgs, ... }:

{
   services.gpg-agent = {
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };
}
