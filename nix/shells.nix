{ config, lib, pkgs, ... }:

let
  shellAliases = {
    l = "${pkgs.exa}/bin/exa";
    ls = "${pkgs.exa}/bin/exa";
    g = "git";
    t = "tig status";
    e = "eval $EDITOR";
    ee = "fzf | xargs $EDITOR";
    download = "aria2c --file-allocation=none --seed-time=0";
    gotty-sridca = "gotty -a 0.0.0.0 -p 9999 -r"; # To be run from the thebeast wireguard peer only.
    youtube-dl-audio = "youtube-dl --ignore-errors --output \"%(title)s.%(ext)s\" --extract-audio --audio-format mp3";
  };

in
{
  home.packages = with pkgs; [
    # Programs used by shell config defined below.
    exa
    bat
    aria
    mosh
    # sshfs -- not available on macOs
    (pkgs.callPackage ./emacs/xterm-24bit.nix {})
  ];

  programs.keychain = {
    enable = true;
    # Disable bash integration which breaks nix-shell everywhere
    enableBashIntegration = false;
    keys = [
      "id_rsa"
      # "/home/srid/work/m/j/sshkey"
    ];
  };

  programs.ssh = {
    enable = true;
    # Keeping individual hosts private.
    matchBlocks = import ../private-config/ssh-match-blocks.nix;
  };

  programs.broot = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };

  programs.fish = {
    inherit shellAliases;
    enable = true;
    shellAbbrs = {
      gs = "git status -s";
      gd = "git diff";
      gca = "git commit --amend";
    };
    plugins = [
      { name = "bobthefish";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "theme-bobthefish";
          rev = "df13338";
          sha256 = "1nyh85ji75j3a23czbaaf9s66dpfhfd9in5b6s5baispi2qfwdh2";
        };
      }
    ];
  };

  # https://www.mathiaspolligkeit.de/dev/exploring-nix-on-macos/
  xdg.configFile."fish/conf.d/plugin-bobthefish.fish".text = lib.mkAfter ''
  for f in $plugin_dir/*.fish
    source $f
  end
  '';

  programs.bash = {
    enable = true;
    historyIgnore = [ "l" "ls" "cd" "exit" ];
    historyControl = [ "erasedups" ];
    enableAutojump = true;
    inherit shellAliases;
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
    defaultCommand = "${pkgs.ripgrep}/bin/rg --files";
  };
}
