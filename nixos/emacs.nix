{ pkgs, ... }: 

let
  myEmacs = pkgs.emacs; 
  emacsWithPackages = (pkgs.emacsPackagesGen myEmacs).emacsWithPackages; 
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [ 
    magit          # ; Integrate git <C-x g>
    zerodark-theme # ; Nicolas' theme
  ]) ++ (with epkgs.melpaPackages; [ 
    # undo-tree      # ; <C-x u> to show the undo tree
    # zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
  ]) ++ (with epkgs.elpaPackages; [ 
    beacon         # ; highlight my cursor when scrolling
  ]) ++ [
    # pkgs.notmuch   # From main packages set 
  ])