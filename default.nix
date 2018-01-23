{ config, pkgs, ...}:

{
  # Firewall
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ 80 443 3000 8080 9812 ];
  networking.firewall.allowPing = true;
  
  programs.mosh.enable = true;
  programs.bash.enableCompletion = true;

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  # Xmonad
  # cf. https://wiki.haskell.org/Xmonad/Installing_xmonad#NixOS
  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
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
    git
    gnumake
    htop
    keychain
    # ripgrep (compiles Rust; disabling temporarily)
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
