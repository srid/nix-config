{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    haskellPackages.hlint
    haskellPackages.stylish-haskell
  ];

  # Gitit is no longer automatically added to the module list in NixOS. So we
  # must import it.
  imports = [ <nixpkgs/nixos/modules/services/misc/gitit.nix> ];
  services.gitit.enable = false;  # Broken

}
