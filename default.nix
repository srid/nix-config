{ config, pkgs, ...}:

{
  programs.mosh.enable = true;

  environment.systemPackages = with pkgs; [
    bash
    exa
    git
    gnumake
    htop
    httpie
    jq
    links
    nix-prefetch-git
    php
    python27Packages.ipython
    ripgrep
    tig
    tmux
    unzip
    vim
    wget

    (emacsWithPackages (with emacsPackagesNg; [
      ace-window
      avy
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
      moe-theme
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
