{ config, lib, pkgs, ... }:

let
  shellAliases = {
    l = "exa";
    ls = "exa";
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
    (pkgs.callPackage ./emacs/xterm-24bit.nix {})
  ];

  programs.keychain = {
    enable = true;
    # Disable bash integration which breaks nix-shell everywhere
    enableBashIntegration = false;
    keys = [
      "id_rsa"
      "/home/srid/work/m/j/sshkey"
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
      gs = "git status";
      gca = "git commit --amend";
    };
  };

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
