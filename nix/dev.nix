{ config, pkgs, ...}:

{
  imports = [ ./gotty.nix ];

  environment.systemPackages = with pkgs; [
    # Haskell stuff
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    haskellPackages.hpack
    python

    emacs26
    asciinema
    tmate

    dropbox-cli
  ];
}
