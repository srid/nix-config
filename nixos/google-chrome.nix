{ config, pkgs, ...}:

{
  # Google Chrome ulimit upping
  # https://bugs.chromium.org/p/chromium/issues/detail?id=362603#c28
  security.pam.loginLimits = [
    { domain = "*"; item = "nofile"; type = "-"; value = "65536"; }
  ];

  environment.systemPackages = with pkgs; [
    google-chrome
    google-chrome-dev
  ];
}
