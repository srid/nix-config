{ pkgs, stdenv, ncurses, ... }:
let
  # https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html
  sourcefile = pkgs.writeText "terminfo-24bit.src" ''
    # Use colon separators.
    xterm-24bit|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
      setf24=\E[38:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
    # Use semicolon separators.
    xterm-24bits|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
      setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
      '';
in
stdenv.mkDerivation {
  name = "xterm-24bit";
  outputs = ["out" "terminfo"];
  builder = pkgs.writeText "builder.sh" ''
      source $stdenv/setup

      mkdir -p $terminfo/share/terminfo
      ${pkgs.ncurses}/bin/tic -x -o $terminfo/share/terminfo ${sourcefile}

      mkdir -p $out/nix-support
      echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
      '';
}
