{ config, lib, pkgs, ... }:

{
   nix.binaryCaches = [
    "https://nixcache.reflex-frp.org"
    "https://srid.cachix.org/"
    "https://all-hies.cachix.org"
    "https://hercules-ci.cachix.org/"
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
    "srid.cachix.org-1:Vq4QePFVPaJt4Zt2SYo1he1uQVJ8pKiCer7Oyg5V6zU="
    "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
  ];
}
