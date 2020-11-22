{ config, pkgs, ...}:

# This module adds a command for controlling screen brightness
# Use as: `sudo xbacklight -set 40`
#
# TODO: make Fn brightness keys work
{
  # Not confirmed if required
  boot.kernelParams = [
    "acpi_backlight=vendor"
    "video.use_native_backlight=1"
  ];

  environment.systemPackages = with pkgs; [ 
    acpi 
    # xorg.xbacklight won't work; we need this
    # cf. https://github.com/NixOS/nixpkgs/issues/71102#issuecomment-542818105
    acpilight
  ];

  services.acpid.enable = true;
}