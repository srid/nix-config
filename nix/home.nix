# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./home/*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, device ? "unknown", ...}:

let
  baseImports = [
    ./home/git.nix
    ./home/haskell.nix
    ./home/shells.nix
    ./home/tmux.nix
  ];
  # For my main development machine only
  thinkpadImports = [
    ./home/scripts.nix

    # i3 and related (not using right now)
    #./home/i3.nix
    #./home/redshift.nix
    #./home/terminal.nix

    #./home/irc.nix
    ./home/HighDpiCursor.nix
    ./home/gpg.nix
    ./home/keybase.nix
    ./home/gotty.nix
    #./home/steam.nix
    ./home/udiskie.nix
    ./home/emacs.nix
    ../private-config/work/aws.nix
  ];
in
{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  imports = if (builtins.currentSystem == "x86_64-linux" && device == "bornagain")
            then (baseImports ++ thinkpadImports)
            else baseImports;

  home.packages = with pkgs; [
    # Basic tools
    htop
    file
    jq
    youtube-dl

    # nvim, and its runtime dependencies
    (callPackage ./nvim {})
    nodejs  # coc.vim requires it

    # Dev tools
    gnumake
    ripgrep
    tig
    tmate
    gitAndTools.gh
    dhall
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };

}
