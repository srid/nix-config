{ config, pkgs, ...}:

{

  imports = [ ./tmux.nix ];

  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowPing = true;

  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

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
    # Essential dev tools
    unstable.emacs26
    gitAndTools.gitFull
    tmate

    # Others
    ag
    asciinema
    bashInteractive
    fzf
    gnumake
    gotty
    htop
    keychain
    psmisc
    python36Packages.glances
    stow
    tig
    tree
    unzip
    wget
    youtube-dl
  ];
}
