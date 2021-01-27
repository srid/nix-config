{ config, lib, pkgs, ... }:

{
  # https://github.com/nix-community/emacs-overlay
  # TODO: Make this a channel?
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  programs.emacs = {
    enable = true;
    # package = pkgs.emacsGit;
    extraPackages = epkgs: with epkgs; [
      use-package

      # Major modes
      nix-mode
      haskell-mode
      # markdown-mode  (using a fork)

      # Git
      magit

      # UX beauty
      all-the-icons-ivy
      doom-modeline
      doom-themes

      # UX behaviours
      # See the following comparison for meta-rationale:
      # https://github.com/raxod502/selectrum/blob/master/README.md#selectrum-in-comparison-to-other-completion-systems
      selectrum
      prescient
      selectrum-prescient
      consult  # https://github.com/minad/consult#available-commands

      # Project
      project

      # VI
      evil
      evil-leader
      which-key
    ];
  };

  services.emacs.enable = true;
}
