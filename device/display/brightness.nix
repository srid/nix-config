{ config, pkgs, ...}:

# This module adds a command for controlling screen brightness
# Use as: 
#   sudo xbacklight -set 40`
# To set brightness on external monitor:
#   ddcutil setvcp 10 50
#
# TODO: make Fn brightness keys work
{
  # Not sure if required
  boot.kernelParams = [
    "acpi_backlight=vendor"
    "video.use_native_backlight=1"
  ];

  environment.systemPackages = with pkgs; [ 
    acpi 
    # xorg.xbacklight won't work; we need this
    # cf. https://github.com/NixOS/nixpkgs/issues/71102#issuecomment-542818105
    acpilight

    # Alternative to xbacklight
    brightnessctl

    # This works with LG Ultrafine 5k
    ddcutil
  ];

  # Obviate sudo
  security.wrappers = {
    ddcutil = { source = "${pkgs.ddcutil}/bin/ddcutil"; };
    xbacklight = { source = "${pkgs.acpilight}/bin/xbacklight"; };
  };
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
  '';

  services.acpid.enable = true;
}