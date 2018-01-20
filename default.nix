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
    stow
    tig
    tmux
    unzip
    vim
    wget

    # Not installing any package as I use spacemacs
    (emacsWithPackages (with emacsPackagesNg; [
    ]))
  ];
}
