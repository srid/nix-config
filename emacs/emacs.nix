{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
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
