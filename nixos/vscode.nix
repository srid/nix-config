{ config, pkgs, lib, ...}:

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

  # If xserver is not enabled, the default for this goes down. Increase it here. VSCode requires it. 
  # https://code.visualstudio.com/docs/setup/linux#_visual-studio-code-is-unable-to-watch-for-file-changes-in-this-large-workspace-error-enospc
  boot.kernel.sysctl."fs.inotify.max_user_watches" = lib.mkDefault 524288;
}
