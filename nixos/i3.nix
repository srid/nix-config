{ config, pkgs, ...}:

{
  services.xserver = {
    windowManager.i3.enable = true;

    desktopManager = {
      # To make home-manager's i3 available in system X session
      # https://discourse.nixos.org/t/opening-i3-from-home-manager-automatically/4849/8
      session = [
        { 
          name = "home-manager";
          start = ''
            ${pkgs.runtimeShell} $HOME/.hm-xsession &
            waitPID=$!
          '';
        }
      ];
    };
  };
}
