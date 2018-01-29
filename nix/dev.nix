{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    haskellPackages.hlint
    haskellPackages.stylish-haskell
  ];
}
