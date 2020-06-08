{ pkgs, ... }:
  let
    # My fork of fx with Nix support
    fxSrc = pkgs.fetchFromGitHub {
      owner = "srid";
      repo = "fx";
      rev = "47e67cb2615832f5c5a871b8a9f58b37294164db";
      sha256 = "sha256:0ld7dm7zhlflcx7c0rm169c8zcvzdj4lvlwdihfgfsji0rp4ad00";
    };
  in
  {
    home.packages = with pkgs; [
      (import fxSrc { }).package
    ];
  }
