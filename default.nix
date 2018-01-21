{ config, pkgs, ...}:

{
  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 80 443 3000 8080 ];
  networking.firewall.allowPing = true;
  
  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;

  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = false;
    windowManager.xmonad = {
      enable = false;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    windowManager.default = "xmonad";
  };

  environment.systemPackages = with pkgs; [
    bash
    # exa -- broken
    git
    gnumake
    htop
    httpie
    jq
    keychain
    links
    nix-prefetch-git
    php
    ripgrep
    stow
    tig
    tmux
    unzip
    vim
    wget

    (emacsWithPackages (with emacsPackagesNg; [
      ace-window
      avy
      counsel
      elm-mode
      evil
      github-theme
      haskell-mode
      ivy
      ivy-hydra
      leuven-theme
      lispy
      magit
      markdown-mode
      material-theme
      nix-mode
      org
      python-mode
      swiper
      worf
      yaml-mode
      zenburn-theme
    ]))
  ];
}
