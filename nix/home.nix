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
  devImports = [
    ./google-cast.nix
    ./keybase.nix

    # Development
    ./haskell.nix

    ./syncthing.nix
  ];
  homeOffice = devImports ++ [
    ./scripts.nix
    #./terminal.nix
    #./irc.nix
    #./gotty.nix
    #./steam.nix
    #./udiskie.nix
    #./i3.nix
    ./redshift.nix
    ../private-config/work/aws.nix

    #./HighDpiCursor.nix
    #./gpg.nix
  ];
in
{
  nixpkgs.config.allowUnfree = true;
  home.file.".config/nixpkgs/config.nix".text = ''
    { allowUnfree = true; }
  '';

  programs.home-manager.enable = true;

  imports = if (hostName == "bornagain")
            then (baseImports ++ homeOffice)
            else if (hostName == "bebe")
              then (baseImports ++ devImports)
              else baseImports;

  home.packages = with pkgs; [
    # To track sources 
    niv

    # Basic tools
    htop
    ytop
    file
    jq
    ncdu
    du-dust
    procs
    sd
    ripgrep
    tealdeer
    # psmisc  # not on macOS
    tree 
    unzip
    wget
    youtube-dl

    # nvim, and its runtime dependencies
    (callPackage ./nvim {})
    nodejs  # coc.vim requires it

    # Dev tools
    gnumake
    tmate
    gitAndTools.gh
    tig
    dhall
    dhall-lsp-server
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };

}
