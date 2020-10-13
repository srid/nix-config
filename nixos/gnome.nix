{ config, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.gdm = {
      enable = true;
      # https://github.com/NixOS/nixpkgs/issues/42053
      autoSuspend = false;
    };
    desktopManager.gnome3 = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    # Tweak tool
    gnome3.gnome-tweaks
    # Extensions
    gnomeExtensions.paperwm
    gnomeExtensions.appindicator
  ];
}
