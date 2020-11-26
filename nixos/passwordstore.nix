{ config, pkgs, ...}:

# Storing passwords using `pass` which requires `gpg`
# TODO: Make this shit work (pinentry nonsense; see issue below)
{
    programs.gnupg = {
      agent = {
        # This SHIT does not work: https://github.com/NixOS/nixpkgs/issues/35464#issuecomment-383894005
        enable = true;
        pinentryFlavor = "curses";
      };
    };
    environment.systemPackages = with pkgs; [
        pass
        gnupg
        pinentry-gnome
    ];
}