{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      magit
      # Major modes
      nix-mode
      markdown-mode
      # UX
      avy
      which-key
      projectile
    ];
  };

  # service.emacs.enable = true;
}
