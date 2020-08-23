{ config, pkgs, ...}:

{
  services.compton = {
    #enable = true;
    #shadow = true;
    #inactiveOpacity = 1.0;
  };

  services.xserver = {
    enable = true;

    layout = "us";
    xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";

    displayManager = {
      hiddenUsers = [ "apps" ];
      gdm = {
        enable = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    asciinema

    # To launch peek, C-p peek
    # C+S+<space> to make it floaitng window
    # Postiion window and begin record
    # C+S+4 to move it hidden i3 workspace
    # When recording is done, hit C+4 to bring it back
    # Stop recording.
    peek
    simplescreenrecorder

    vscode
    xorg.xbacklight
    xorg.xf86videointel
    xorg.xprop
    xsel
  ];
}
