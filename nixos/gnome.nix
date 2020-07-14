{ config, pkgs, ...}:

{
  services.xserver = {
    desktopManager.gnome3 = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
  ];
}
