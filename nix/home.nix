# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file should work across all of my computing devices. 
# Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, ...}:

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
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    (callPackage (import ./nvim/default.nix) {})
    dejavu_fonts
    source-serif-pro
    #emacs  -- Don't want emacs on crostini
    #sqlite gcc  # For emacsql
    aria
    cachix
    htop
    coin
    dict
    exa
    file
    fortune
    gron
    ii # suckless irc client
    mosh
    mpv
    ripgrep
    sshfs
    taskwarrior
    tig
    transmission
    wireguard
    youtube-dl
    haskellPackages.pandoc
  ];

  home.sessionVariables = {
    # https://github.com/syl20bnr/spacemacs/wiki/Terminal
    # TERM = "xterm-24bit";  breaks on crostini
    EDITOR = "nvim";
  };

  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 128;
  };

  programs.tmux = {
    enable = true;
    shortcut = "a"; # Use Ctrl-a 
    baseIndex = 1; # Widows numbers begin with 1
    keyMode = "vi";
    customPaneNavigationAndResize = true;
    aggressiveResize = true;
    historyLimit = 100000;
    resizeAmount = 5;
    escapeTime = 0;

    extraConfig = ''
      # Fix environment variables
      set-option -g update-environment "SSH_AUTH_SOCK \
                                        SSH_CONNECTION \
                                        DISPLAY"

      # Mouse works as expected
      set-option -g mouse on

      # Use default shell 
      set-option -g default-shell ''${SHELL}
      # set -g default-terminal "xterm-24bit"

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

  programs.fish = {
    enable = true;
    shellAliases = {
      l = "exa";
      ls = "exa";
      copy = "xclip -i -selection clipboard";
      g = "git";
      e = "eval $EDITOR";
      ee = "e (fzf)";
      download = "aria2c --file-allocation=none --seed-time=0";
      chromecast = "castnow --address 192.168.2.64 --myip 192.168.2.76";
      gotty-sridca = "gotty -a 0.0.0.0 -p 9999 -r"; # To be run from the thebeast wireguard peer only.
    };
  };

  programs.bash = {
    enable = true;
    historyIgnore = [ "l" "ls" "cd" "exit" ];
    historyControl = [ "erasedups" ];
    enableAutojump = true;
    shellAliases = {
      l = "exa";
      ls = "exa";
      copy = "xclip -i -selection clipboard";
      g = "git";
      e = "$EDITOR";
      ee = "e $(fzf)";
      download = "aria2c --file-allocation=none --seed-time=0";
      chromecast = "castnow --address 192.168.2.64 --myip 192.168.2.76";
    };
    initExtra = ''
    if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then 
      . ~/.nix-profile/etc/profile.d/nix.sh; 
      export NIX_PATH=$HOME/.nix-defexpr/channels''${NIX_PATH:+:}$NIX_PATH
    fi # added by Nix installer
    '';
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
        editor = "nvim";
      };
    };
  };
  programs.command-not-found.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  # TODO: Don't enable these on crostini

  # Automounter for removable media.
  services.udiskie = {
    enable = true;
    automount = true;
    notify = true;
  };

  services.redshift = {
    enable = true;
    tray = true;
    # Quebec City
    latitude = "46.8423";
    longitude = "-71.2429";
  };

  home.file = {
    ".stylish-haskell.yaml".source = ../stylish-haskell.yaml;
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
