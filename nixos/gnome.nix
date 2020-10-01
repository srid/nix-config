{ config, pkgs, ...}:

{
  services.xserver = {
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
    gnomeExtensions.material-shell
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
