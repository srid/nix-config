# https://nixos.wiki/wiki/Home_Manager

{ config, pkgs, ...}:

{
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];
  home-manager.users.srid = {
    home.packages = with pkgs; [
      fortune
      dict
      (callPackage (import ./nvim/default.nix) {})

      # Collaboration tools
      termtosvg
    ];

    home.sessionVariables = {
      TERM = "xterm-256color";
      EDITOR = "nvim";
    };

    programs.bash = {
      enable = true;
      historyIgnore = [ "ls" "cd" "exit" ];
      historyControl = [ "erasedups" ];
      enableAutojump = true;
      shellAliases = {
        copy = "xclip -i -selection clipboard";
      };
      # TODO: profileExtra
    };
    programs.fzf = {
      enable = true;
      enableBashIntegration = true;
    };
    programs.git = {
      package = pkgs.gitAndTools.gitFull;
      enable = true;
      userName = "Sridhar Ratnakumar";
      userEmail = "srid@srid.ca";
      ignores = [ "*~" "*ghcid.txt" ];
      aliases = {
        co = "checkout";
        ci = "commit";
      };
      extraConfig = {
        core = {
          editor = "nvim";
        };
      };
    };
    programs.command-not-found.enable = true;

    services.screen-locker = {
      enable = true;
      inactiveInterval = 3;
    };

    home.file = {
      ".stylish-haskell.yaml".source = ../stylish-haskell.yaml;
      ".spacemacs".source = ../spacemacs;
      ".ghci".text = ''
        :set prompt "λ> "
      '';
    };

  };
}
