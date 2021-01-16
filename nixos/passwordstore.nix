{ config, pkgs, ...}:

# Storing passwords using `pass` which requires `gpg`
{
    # Must restart computer, otherwise you may hit this bug:
    # https://github.com/NixOS/nixpkgs/issues/35464#issuecomment-383894005
    programs.gnupg = {
      agent = {
        enable = true;
        enableExtraSocket = true;
        pinentryFlavor = "curses";
      };
    };
    environment.systemPackages = with pkgs; [
      pass
    ];
}
