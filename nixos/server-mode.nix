{ config, pkgs, ... }:

{
  services.tlp = {
    # NOTE: disalbing tlp because I want to ignore suspend on lid. Ideally
    # disable just that. P71 is acting as a server right now, always plugged.
    enable = false;
  };
  # This machine is now a long-running home-server with a bluetooth keyboard
  services.logind.lidSwitch = "ignore";
}
