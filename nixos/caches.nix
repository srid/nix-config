{ config, lib, pkgs, ... }:

{
  # NOTE: These caches are used on NixOS (nixos-rebuild) only, and not in
  # home-manager (which would only use the user's nix.conf).

  nix.binaryCaches = [
    "https://nixcache.reflex-frp.org"
    "https://srid.cachix.org/"
    "https://all-hies.cachix.org"
    "https://hercules-ci.cachix.org/"
    "https://ghcide-nix.cachix.org/"
  ];
  # nix.trustedBinaryCaches = [
  #   "https://cachix.cachix.org"
  #   "https://nixcache.reflex-frp.org"
  #   "https://srid.cachix.org/"
  #   "https://all-hies.cachix.org"
  #   "https://hercules-ci.cachix.org/"
  # ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
    "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI="
    "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    "ghcide-nix.cachix.org-1:ibAY5FD+XWLzbLr8fxK6n8fL9zZe7jS+gYeyxyWYK5c="
  ];
}
