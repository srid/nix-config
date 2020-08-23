{ config, pkgs, ...}:

{
  services.xserver = {
    desktopManager.plasma5 = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
  ];
}
