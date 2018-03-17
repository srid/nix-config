{ config, pkgs, ...}:

{

  imports = [ ./tmux.nix ];

  time.timeZone = "America/New_York";

  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 80 443 3000 3001 8080 9812 ];
  networking.firewall.allowPing = true;

  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  fonts.fonts = with pkgs; [
    fira-code
    google-fonts
    hasklig
    iosevka
    powerline-fonts
    emacs-all-the-icons-fonts
    font-awesome-ttf
    dejavu_fonts
  ];

  environment.systemPackages = with pkgs; [
    # more fonts

    ag
    bashInteractive
    fzf
    gnumake
    htop
    keychain
    stow
    tig
    tree
    unzip
    wget
    psmisc
    asciinema
    gotty
  ];
}
