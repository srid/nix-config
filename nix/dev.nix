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
    vscode
    python

    emacs26
    asciinema
    tmate

    dropbox-cli
    # androidsdk
  ];

  # nixpkgs.config = {
  #  android_sdk.accept_license = true;
  # };

  services.udev.packages = [ pkgs.android-udev-rules ];

  services.ihaskell = {
    enable = false;
    extraPackages = haskellPackages: [ 
      haskellPackages.typed-process
    ];
  };
}
