{ config, pkgs, ...}:

{
  programs.home-manager.enable = true;

  programs.ssh = {
    enable = true;
    # https://nixos.wiki/wiki/Distributed_build
    matchBlocks = import ../private-config/ssh-match-blocks.nix;
  };
}
