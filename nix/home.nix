# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./home/*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

device: { config, pkgs, ...}:

let
  baseImports = [
    ./home/git.nix
    ./home/haskell.nix
    ./home/shells.nix
    ./home/tmux.nix
    ./home/fx.nix
  ];
  # For my main development machine only
  thinkpadImports = [
    ./home/scripts.nix
    ./home/terminal.nix
    ./home/i3.nix
    ./home/irc.nix
    ./home/HighDpiCursor.nix
    ./home/gpg.nix
    ./home/redshift.nix
    ./home/keybase.nix
    ./home/gotty.nix
    # ./home/steam.nix
    ./home/udiskie.nix
    ./home/emacs.nix
    ../private-config/work/aws.nix
  ];
in
{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  imports = if (builtins.currentSystem == "x86_64-linux" && device == "thebeast")
            then (baseImports ++ thinkpadImports)
            else baseImports;

  home.packages = with pkgs; [
    # Basic tools
    htop
    file
    cmake # For emacs config
    gcc
    libtool
    jq
    sshfs
    haskellPackages.mmark-cli

    # nvim, and its runtime dependencies
    (callPackage ./nvim {})
    nodejs  # coc.vim requires it
    xclip  # config.vim references it

    # Dev tools
    gnumake
    ripgrep
    ag
    tig
    tmate
    gitAndTools.gh
    dhall

    # Media
    mpv
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "myst";
  };

}
