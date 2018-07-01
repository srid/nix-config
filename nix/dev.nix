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
    haskellPackages.hpack
    python

    unstable.emacs26
    gitAndTools.gitFull
    asciinema

    (callPackage (import ./nvim/default.nix) {})
  ];

  services.ihaskell = {
    enable = true;
    extraPackages = haskellPackages: [ 
      haskellPackages.typed-process
    ];
  };
}
