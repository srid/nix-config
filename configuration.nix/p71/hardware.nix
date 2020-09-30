{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixos-hardware/lenovo/thinkpad>
      # Assuming on blind faith that is okay for p71
      <nixos-hardware/lenovo/thinkpad/p53>
      # Pull in hardware specific modules when relevant
      <nixos-hardware/common/pc/ssd>
    ];

  # This machine is now a long-running home-server with a bluetooth keyboard
  services.logind.lidSwitch = "ignore";

  sound.mediaKeys.enable = true;

  # For Google's Titan key
  services.udev.packages = [ 
    pkgs.libfido2 
    pkgs.android-udev-rules 
  ];

  # Fingerprint reader
  services.fwupd.enable = true;
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.xscreensaver.fprintAuth = true;

  # TLP Linux Advanced Power Management
  # Seems to make suspend / wake-up work on lid-close.
  services.tlp = {
    # NOTE: disalbing tlp because I want to ignore suspend on lid. Ideally
    # disable just that. P71 is acting as a server right now, always plugged.
    enable = false;
    settings = {
      CPU_MAX_PERF_ON_BAT=60;
      CPU_HWP_ON_BAT="balance_power";
    };
  };
}
