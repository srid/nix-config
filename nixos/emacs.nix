{ pkgs ? import <nixpkgs> {} }: 

let
  myEmacs = pkgs.emacs; 
  emacsWithPackages = (pkgs.emacsPackagesGen myEmacs).emacsWithPackages; 
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [ 
    magit          # ; Integrate git <C-x g>
    nix-mode
    markdown-mode
  ]) ++ (with epkgs.melpaPackages; [ 
    # undo-tree      # ; <C-x u> to show the undo tree
    # zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
  ]) ++ (with epkgs.elpaPackages; [ 
    # auctex         # ; LaTeX mode
    # beacon         # ; highlight my cursor when scrolling
    # nameless       # ; hide current package name everywhere in elisp code
  ]) ++ [
    # pkgs.notmuch   # From main packages set 
  ])
