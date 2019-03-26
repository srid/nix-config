# NOTE: base.nix should be as lean as possible
{ config, pkgs, ...}:

{

  imports = [ ./tmux.nix ./gotty.nix ];

  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
  networking.firewall.allowPing = true;

  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;
  # programs.fish.enable = true;

  nix.trustedUsers = [ "root" "srid" ];
  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "https://srid.cachix.org/"];
  nix.trustedBinaryCaches = [ "https://cachix.cachix.org" "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "https://srid.cachix.org/" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" "srid.cachix.org-1:Vq4QePFVPaJt4Zt2SYo1he1uQVJ8pKiCer7Oyg5V6zU=" "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="];

  nixpkgs.config = {
    allowUnfree = true;

    # Create an alias for the unstable channel
    packageOverrides = pkgs: {
      unstable = import <nixpkgs-unstable> {
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    git
    vim
    gnumake

    # Used by oh-my-fish plugins
    keychain
    jq
    bind

    openssl
    gocryptfs
    ncdu
    ag
    ripgrep
    htop
    psmisc
    stow
    tree
    unzip
    wget
  ];
}
