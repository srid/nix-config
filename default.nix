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
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = with pkgs; (emacsWithPackages (with emacsPackagesNg; [
      ace-window
      avy
      beacon         # ; highlight my cursor when scrolling
      counsel
      elm-mode
      evil
      github-theme
      haskell-mode
      ivy
      ivy-hydra
      leuven-theme
      lispy
      magit          # ; Integrate git <C-x g>
      markdown-mode
      material-theme
      nix-mode
      org
      pdf-tools
      python-mode
      swiper
      undo-tree      # ; <C-x u> to show the undo tree
      worf
      yaml-mode
      zenburn-theme
      # zerodark-theme -- fails due to checksum mismatch on font-lock+.el
    ]));
  };
}
