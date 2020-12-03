{ pkgs, ...}:

# Get the fucking basics right, motherfucker
{
  environment.systemPackages = with pkgs; [
    nix-du
    lsof 
    usbutils
    pciutils
    arandr
  ];
}
