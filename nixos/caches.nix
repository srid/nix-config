{ config, lib, pkgs, ... }:

{
  # NOTE: These caches are used on NixOS (nixos-rebuild) only, and not in
  # home-manager (which would only use the user's nix.conf).

  nix.binaryCaches = [
    "https://nixcache.reflex-frp.org"
    "https://srid.cachix.org/"
  ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI="
  ];
}
