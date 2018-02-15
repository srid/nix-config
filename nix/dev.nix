{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    # Haskell stuff
    cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.stylish-haskell

    gitAndTools.hub
    tmate
  ];

  # Gitit is no longer automatically added to the module list in NixOS. So we
  # must import it.
  imports = [ <nixpkgs/nixos/modules/services/misc/gitit.nix> ];
  services.gitit.enable = false;  # Broken

}
