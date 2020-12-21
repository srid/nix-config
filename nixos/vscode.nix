{ config, pkgs, ...}:

let 
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  environment.systemPackages = with pkgs; [
    nixpkgs-master.vscode
    # Needed for vscode-remote extension per
    # https://nixos.wiki/wiki/Vscode
    nodejs-12_x
  ];

  # For vscode to store secrets
  # Provide org.freedesktop.secrets 
  services.gnome3.gnome-keyring.enable = true;
}
