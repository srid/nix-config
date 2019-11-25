# NOTE: base.nix should be as lean as possible
{ config, pkgs, ...}:

{

  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
  networking.firewall.allowPing = true;

  nix.trustedUsers = [ "root" "srid" ];
  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "https://srid.cachix.org/" "https://all-hies.cachix.org"];
  nix.trustedBinaryCaches = [ "https://cachix.cachix.org" "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "https://srid.cachix.org/" "https://all-hies.cachix.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" "srid.cachix.org-1:Vq4QePFVPaJt4Zt2SYo1he1uQVJ8pKiCer7Oyg5V6zU=" "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=" "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="];

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
