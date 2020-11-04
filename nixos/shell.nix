{ config, lib, pkgs, ... }:

let
  shellAliases = {
    l = "${pkgs.exa}/bin/exa";
    g = "git";
    t = "tig status";
    e = "emacsclient";
    ee = "fzf | xargs emacsclient";
    download = "aria2c --file-allocation=none --seed-time=0";
    # gotty-sridca = "gotty -a 0.0.0.0 -p 9999 -r"; # To be run from the thebeast wireguard peer only.
    youtube-dl-audio = "youtube-dl --ignore-errors --output \"%(title)s.%(ext)s\" --extract-audio --audio-format mp3";
    cast-video = "${pkgs.catt}/bin/catt -d SridScreen cast";
    cast-audio = "${pkgs.catt}/bin/catt -d SridMini cast";
  };

in
{
  environment.systemPackages = with pkgs; [
    fzf
    (pkgs.callPackage ../nix/xterm-24bit.nix {})
  ];

  programs.autojump.enable = true;

  programs.bash = {
    inherit shellAliases;
    enableCompletion = true;
    enableLsColors = true; 
  };
}
