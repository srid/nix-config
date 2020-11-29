{ pkgs, ...}:

# Get the fucking basics right, motherfucker
{
  environment.systemPackages = with pkgs; [
    lsof 
    pciutils
    arandr
  ];
}