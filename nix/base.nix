{ config, pkgs, ...}:

{
  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 80 443 3000 8080 9812 ];
  networking.firewall.allowPing = true;
  
  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  fonts.fonts = with pkgs; [
    google-fonts
  ];

  environment.systemPackages = with pkgs; [
    ag
    bash
    git
    gnumake
    htop
    keychain
    stow
    tig
    tmux
    tree
    unzip
    vim
    wget
  ];
}
