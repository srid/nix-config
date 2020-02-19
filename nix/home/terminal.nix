{ config, pkgs, ...}:

let 
  # Suckless Terminal provides a good performance. Just need to increase the
  # fontsize on retina display.
  myst = pkgs.writeScriptBin "myst" 
  ''
    #!${pkgs.runtimeShell}
    exec ${pkgs.st}/bin/st -f "monospace:pixelsize=24"
  '';
in 
{
  home.packages = with pkgs; [
    myst
  ];
}

