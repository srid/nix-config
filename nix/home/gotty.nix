{ pkgs, fetchGH, ... }:
  let
    # My fork of gotty fixing websocket issue.
    gotty = fetchGH "srid/gotty" "a9afda38b263dbaa96a2a4cf9fd19d4c9a27213c";
  in
  {
    home.packages = with pkgs; [
      (callPackage gotty {})
    ];
  }
