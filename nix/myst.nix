{pkgs, ...}:

# Suckless Terminal provides good performance. Just need to increase the
# fontsize on retina display.
pkgs.writeScriptBin "myst" 
''
  #!${pkgs.runtimeShell}
  # Use fc-list to lookup font names
  exec ${pkgs.st}/bin/st -f "CascadiaCode:pixelsize=26" $*
''

