{ config, pkgs, ...}:

{
  programs.home-manager.enable = true;

  programs.ssh = {
    enable = true;
    matchBlocks = import ../private-config/ssh-match-blocks.nix;
  };
}
