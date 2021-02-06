{ config, pkgs, lib, ...}:

{
  home.packages = with pkgs; [
    # Needed for vscode-remote extension per
    # https://nixos.wiki/wiki/Vscode
    nodejs-12_x
  ];

  programs.vscode = {
    enable = true;
  };

}
