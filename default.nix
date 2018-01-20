{ config, pkgs, ...}:

{
  programs.mosh.enable = false;

  virtualisation.docker.enable = true;

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
