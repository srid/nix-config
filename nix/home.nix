# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file, and ./*.nix, should work across all of my computing
# devices. Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, hostName ? "unknown", ...}:

{
  programs.home-manager.enable = true;
  
  nixpkgs.config.allowUnfree = true;
  home.file.".config/nixpkgs/config.nix".text = ''
    { allowUnfree = true; }
  '';

  imports = 
    if (hostName == "bornagain" || hostName == "bebe")
      then [
        ./keybase.nix 
        ./haskell.nix 
        ./i3-config.nix
        ./doom-emacs.nix
      ]
      else [];

  home.packages = with pkgs; [
    # Basic tools
    htop
    nnn
    file
    jq
    ncdu
    procs
    sd
    ripgrep
    psmisc  # not on macOS
    tree 
    unzip
    wget
    youtube-dl

    # nvim, and its runtime dependencies
    (callPackage ./nvim {})

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

  programs.ssh = {
    enable = true;
    # https://nixos.wiki/wiki/Distributed_build
    # TODO: Move to nixos: programs.ssh.knownHosts?
    matchBlocks = import ../private-config/ssh-match-blocks.nix;
  };

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Sridhar Ratnakumar";
    userEmail = "srid@srid.ca";
    aliases = {
      co = "checkout";
      ci = "commit";
      s = "status";
      st = "status";
    };
    extraConfig = {
      core.editor = "nvim";
      protocol.keybase.allow = "always";
      credential.helper = "store --file ~/.git-credentials";
      pull.rebase = "false";
    };
  };

}
