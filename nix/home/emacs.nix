{ pkgs, ... }:
  let
    emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages;
    # elpa vs melpa vs melpaStable: https://emacs.stackexchange.com/a/10501/2059
    myEmacs = emacsWithPackages (epkgs: [
      # epkgs.emacs-libvterm
    ]);
  in
  {
    home.packages = with pkgs; [
      myEmacs
    ];
  }
