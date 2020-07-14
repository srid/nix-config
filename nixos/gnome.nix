{ config, pkgs, ...}:

{
  services.xserver = {
    desktopManager.gnome3 = {
      enable = true;
    };
  };

  services.gnome3 = {
    tracker-miners.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnomeExtensions.paperwm
  ];
}
