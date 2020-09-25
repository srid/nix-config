{ config, pkgs, ...}:

{
  services.xserver = {
    displayManager.gdm.enable = true;
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
    # Themes
    materia-theme
    equilux-theme
    qogir-theme
    theme-obsidian2
    adementary-theme
    plata-theme
    arc-theme

    # Twitter 
    cawbird
  ];
}
