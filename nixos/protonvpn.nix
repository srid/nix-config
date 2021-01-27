{ pkgs, ...}:

# Initial configure on terminal via:
#   $ protonvpn init
# Use with this GNOME extension:
#   https://extensions.gnome.org/extension/3133/protonvpn-status/
{
  environment.systemPackages = with pkgs; [
    protonvpn-cli
  ];

  security.sudo.extraRules = [
    { users = [ "srid" ];
      commands = [
        { command = "${pkgs.protonvpn-cli}/bin/protonvpn";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
