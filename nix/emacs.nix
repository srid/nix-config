{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      use-package
      use-package-ensure-system-package
      # Major modes
      nix-mode
      markdown-mode
      # Git
      magit
      # UX beauty
      all-the-icons-ivy
      doom-modeline
      doom-themes
      # UX behaviours
      avy
      which-key
      projectile
      # Project
      #treemacs
      #treemacs-projectile
      #treemacs-evil
      # VI
      evil
    ];
  };

  # service.emacs.enable = true;
}
