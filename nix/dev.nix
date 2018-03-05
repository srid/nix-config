{ config, pkgs, ...}:

let

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      twitch-cli = self.callCabal2nix "twitch-cli" (pkgs.fetchFromGitHub {
        owner  = "3noch";
        repo   = "twitch-cli";
        rev    = "85047186c3790ab8f015bdc4658abfe63c6129b7";
        sha256 = "1yr53r3h0p12dj2cyc3j6r71nyf0g93x1xbra9205f6qp3ymc205";
      }) {};
    };
  };

in

{
  environment.systemPackages = with pkgs; [
    # Haskell stuff
    cabal-install
    cabal2nix
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    myHaskellPackages.twitch-cli
    python

    gitAndTools.gitFull
    tmate

    (callPackage (import ./nvim/default.nix) {})
  ];

  # Gitit is no longer automatically added to the module list in NixOS. So we
  # must import it.
  imports = [ <nixpkgs/nixos/modules/services/misc/gitit.nix> ];

  services = {
    # gitit.enable = true; -- broken

    ihaskell = {
      enable = true;
    };

    postgresql.enable = true;
  };
}
