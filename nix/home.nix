# https://nixos.wiki/wiki/Home_Manager

{ config, pkgs, hostName ? "unknown", ...}:

{
  programs.home-manager.enable = true;
  
  nixpkgs.config.allowUnfree = true;
  home.file.".config/nixpkgs/config.nix".text = ''
    { allowUnfree = true; }
  '';

  imports = 
    if (hostName == "thebeast" || hostName == "bebe")
      then [
        ../nix/keybase.nix 
        ../nix/vscode.nix 
        #../emacs/emacs.nix
        #../nix/doom-emacs.nix
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
    tree 
    unzip
    wget
    youtube-dl
    cachix
    ffmpeg

    # nvim, and its runtime dependencies
    (callPackage ../nix/nvim {})

    # Dev tools
    gnumake
    tmate
    gitAndTools.gh
    tig

    fzf
    psmisc  # For killall
    (pkgs.callPackage ./xterm-24bit.nix {})
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs.bash = {
    enable = true;
    shellAliases = import ./shellAliases.nix { inherit pkgs; };
  };

  programs.autojump.enable = true;

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
    ignores = [ "*~" "*.swp" ];
    extraConfig = {
      init.defaultBranch = "master";
      core.editor = "nvim";
      protocol.keybase.allow = "always";
      credential.helper = "store --file ~/.git-credentials";
      pull.rebase = "false";
    };
  };

}
