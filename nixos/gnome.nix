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

  services.flatpak.enable = true;

  environment.systemPackages = with pkgs; [
    # Tweak tool
    gnome3.gnome-tweaks
    # Extensions
    gnomeExtensions.paperwm
    gnomeExtensions.appindicator
  ];

  # Fuckin european sleep slut fuck
  systemd.targets.sleep.enable = true;
  systemd.targets.suspend.enable = true;
  systemd.targets.hibernate.enable = false;
  systemd.targets.hybrid-sleep.enable = false;
}
