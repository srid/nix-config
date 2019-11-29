# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./home/*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, ...}:

let
  allPlatformImports = [
    ./home/git.nix
    ./home/haskell.nix
    ./home/shells.nix
    ./home/tmux.nix
  ];
  linuxImports = [
    ./home/HighDpiCursor.nix
    ./home/gpg.nix
    ./home/redshift.nix
    ./home/keybase.nix
    ./home/gotty.nix
    ./home/steam.nix
  ];
in
{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  # Utility functions
  _module.args = {
    fetchGH = fq: rev: builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  };

  imports = if builtins.currentSystem == "x86_64-linux"
            then (allPlatformImports ++ linuxImports)
            else allPlatformImports;

  home.packages = with pkgs; [
    (callPackage ./nvim.nix {})
    htop
    file

    mpv

    # Dev tools
    gnumake
    ripgrep
    tig
    tmate

    # Emacs
    emacs26
    # wordnet
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
