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

        # Collaboration tools
        termtosvg

        taskwarrior
        coin
      ];

      home.sessionVariables = {
        # TERM = "xterm-256color";
        EDITOR = "nvim";
      };

      programs.bash = {
        enable = true;
        historyIgnore = [ "ls" "cd" "exit" ];
        historyControl = [ "erasedups" ];
        enableAutojump = true;
        shellAliases = {
          copy = "xclip -i -selection clipboard";
          g = "git";
        };
        # TODO: profileExtra
      };
      programs.fzf = {
        enable = true;
        enableBashIntegration = true;
      };
      programs.tmux = {
        enable = true;
        plugins = with pkgs; [
            tmuxPlugins.cpu
            {
              plugin = tmuxPlugins.resurrect;
              extraConfig = "set -g @resurrect-strategy-nvim 'session'";
            }
            {
              plugin = tmuxPlugins.continuum;
              extraConfig = ''
                set -g @continuum-restore 'on'
                set -g @continuum-save-interval '60' # minutes
              '';
            }
          ];
        extraConfig = ''
          # Fix environment variables
          set-option -g update-environment "SSH_AUTH_SOCK \
                                            SSH_CONNECTION \
                                            DISPLAY"

          # Mouse works as expected
          set-option -g mouse on

          # Use default shell 
          set-option -g default-shell ''${SHELL}

          # Extra Vi friendly stuff
          # y and p as in vim
          bind Escape copy-mode
          unbind p
          bind p paste-buffer
          bind-key -T copy-mode-vi 'v' send -X begin-selection
          bind-key -T copy-mode-vi 'C-v' send -X rectangle-toggle
          #bind-key -T copy-mode-vi 'y' send -X copy-pipe-and-cancel
          bind-key -T copy-mode-vi 'y' send -X copy-pipe-and-cancel 'xclip -in -selection clipboard'
          bind-key -T copy-mode-vi 'Space' send -X halfpage-down
          bind-key -T copy-mode-vi 'Bspace' send -X halfpage-up
          bind-key -Tcopy-mode-vi 'Escape' send -X cancel

          # easy-to-remember split pane commands
          bind | split-window -h -c "#{pane_current_path}"
          bind - split-window -v -c "#{pane_current_path}"
          bind '"' split-window -h -c "#{pane_current_path}"
          bind % split-window -v -c "#{pane_current_path}"
          bind c new-window -c "#{pane_current_path}"
          #unbind '"'
          #unbind %

        '';

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
          pr = "pull --rebase";
          l = "log --graph --pretty='%Cred%h%Creset - %C(bold blue)<%an>%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)' --abbrev-commit --date=relative";
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
