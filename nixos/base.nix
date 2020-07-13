# NOTE: base.nix should be as lean as possible
{ config, pkgs, ...}:

{

  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
  networking.firewall.allowPing = true;

  nix.trustedUsers = [ "root" "srid" ];

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    ncdu
    psmisc
    stow
    tree
    unzip
    wget
  ];
}
