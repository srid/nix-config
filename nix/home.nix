# https://nixos.wiki/wiki/Home_Manager

{ config, pkgs, ...}:

{
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];
  home-manager.users.srid =
    let
      coinSound = pkgs.fetchurl {
        url = "https://themushroomkingdom.net/sounds/wav/smw/smw_coin.wav";
        sha256 = "18c7dfhkaz9ybp3m52n1is9nmmkq18b1i82g6vgzy7cbr2y07h93";
      };
      coin = pkgs.writeShellScriptBin "coin" ''
        ${pkgs.sox}/bin/play --no-show-progress ${coinSound}
      '';
    in
    {
      home.packages = with pkgs; [
        fortune
        dict
        (callPackage (import ./nvim/default.nix) {})
        cachix
        transmission
        # Collaboration tools
        # termtosvg -- fails on unstable
        taskwarrior
        coin
        wireguard
        gron
        mpv
        file
        sshfs
        aria
      ];

      home.sessionVariables = {
        # TERM = "xterm-256color";
        EDITOR = "emacs -nw";
      };

      programs.bash = {
        enable = true;
        historyIgnore = [ "ls" "cd" "exit" "wget" ];
        historyControl = [ "erasedups" ];
        enableAutojump = true;
        shellAliases = {
          copy = "xclip -i -selection clipboard";
          g = "git";
          e = "emacs -nw";
          ee = "emacs -nw $(fzf)";
          download = "aria2c --file-allocation=none";
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
          s = "status";
          st = "status";
          d = "diff";
          pr = "pull --rebase";
          l = "log --graph --pretty='%Cred%h%Creset - %C(bold blue)<%an>%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)' --abbrev-commit --date=relative";
        };
        extraConfig = {
          core = {
            editor = "emacs -nw";
          };
        };
      };
      programs.command-not-found.enable = true;

      services.screen-locker = {
        enable = true;
        inactiveInterval = 3;
      };
      services.gpg-agent = {
        enable = true;
        defaultCacheTtl = 1800;
        enableSshSupport = true;
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
