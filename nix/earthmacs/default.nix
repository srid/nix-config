/*
 Emacs *not* on space.
 No spacemacs here.
 Keep It Simple Stupid.
*/

{ pkgs ? import <nixpkgs> {} }:

let
  earthmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen earthmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    zerodark-theme
  ]) ++ (with epkgs.melpaPackages; [
    undo-tree
    haskell-mode
  ]) ++ (with epkgs.elpaPackages; [
  
  ]) ++ [

  ])
