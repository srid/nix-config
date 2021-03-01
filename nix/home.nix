# https://nixos.wiki/wiki/Home_Manager

{ config, pkgs, hostName ? "unknown", ...}:

let 
  term = import ./myst.nix { inherit pkgs; };
  termBin = "${term}/bin/myst";
in
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
      else [
        ../nix/vscode.nix
      ];

  home.packages = with pkgs; [
    cachix
    dmenu
    ffmpeg
    file
    fzf
    gitAndTools.gh
    gnumake
    htop
    jq
    ncdu  # CLI disk usage analyzer
    psmisc  # For killall
    ripgrep
    sd
    term
    tig
    tmate
    tree 
    unzip
    wget
    youtube-dl

    # nvim, and its runtime dependencies
    (callPackage ./nvim {})
    (pkgs.callPackage ./xterm-24bit.nix {})

  ];

  home.sessionVariables = {
    EDITOR = "nvim";
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
      cia = "commit --amend";
      s = "status";
      st = "status";
      b = "branch";
      p = "pull --rebase";
      pu = "push";
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

  programs.rofi = {
    enable = true;
    font = "Monospace 24"; /* For larger DPIs */
    padding = 10;
    theme = "fancy";
    # Uhh, https://github.com/davatorium/rofi/pull/1074
    # terminal = "${pkgs.gnome3.gnome_terminal}/bin/gnome-terminal";
    terminal = termBin;
  };

}
