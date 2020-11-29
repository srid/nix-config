{ config, pkgs, ...}:

# Storing passwords using `pass` which requires `gpg`
# TODO: understand machinary of pass/gpg interaction
# TODO: find mobile app
{
    # Must restart computer, otherwise you may hit this bug:
    # https://github.com/NixOS/nixpkgs/issues/35464#issuecomment-383894005
    programs.gnupg = {
      agent = {
        enable = true;
        # Not sure what this is for; I enabled it to debug gpg issues
        enableExtraSocket = true;
        pinentryFlavor = "curses";
      };
    };
    environment.systemPackages = with pkgs; [
        pass
    ];
}