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

  fonts.fonts = with pkgs; [
    dejavu_fonts
    emacs-all-the-icons-fonts
    fira-code
    font-awesome-ttf
    google-fonts
    hasklig
    iosevka
    powerline-fonts
  ];

  environment.systemPackages = with pkgs; [
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
