# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./home/*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, ...}:

let
  unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
in
{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  # Utility functions
  _module.args = {
    fetchGH = fq: rev: builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  };

  imports = [
    ./home/HighDpiCursor.nix
    ./home/git.nix
    ./home/gotty.nix
    ./home/gpg.nix
    ./home/haskell.nix
    ./home/keybase.nix
    ./home/shells.nix
    ./home/tmux.nix
  ];

  home.packages = with pkgs; [
    (callPackage ./nvim.nix {})
    htop
    file

    mpv
    unstable.steam

    # Dev tools
    gnumake
    ripgrep
    tig
    tmate
    emacs26
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  # Automounter for removable media.
  services.udiskie = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    automount = true;
    notify = true;
  };
}
