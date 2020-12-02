{ pkgs, ...}:

# Get the fucking basics right, motherfucker
{
  environment.systemPackages = with pkgs; [
    lsof 
    usbutils
    pciutils
    arandr
  ];
}
