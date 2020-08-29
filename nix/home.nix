# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, hostName ? "unknown", ...}:

let
  baseImports = [
    ./git.nix
    ./shells.nix
    ./tmux.nix
  ];
  # For my main development machine only
  thinkpadImports = [
    ./scripts.nix

    # i3 and related
    ./i3.nix
    ./redshift.nix
    ./terminal.nix

    #./irc.nix
    ./google-cast.nix
    ./HighDpiCursor.nix
    ./gpg.nix
    ./keybase.nix
    ./gotty.nix
    #./steam.nix
    ./udiskie.nix
    ../private-config/work/aws.nix
 
    # Development
    ./haskell.nix
  ];
in
{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  imports = if (builtins.currentSystem == "x86_64-linux" && hostName == "bornagain")
            then (baseImports ++ thinkpadImports)
            else baseImports;

  home.packages = with pkgs; [
    # To track sources 
    niv

    # Basic tools
    htop
    file
    jq
    ncdu
    psmisc
    tree 
    unzip
    wget
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
