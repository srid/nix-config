{ config, lib, pkgs, ... }:

{
   fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      # This is supposed to give us emoji fonts
      noto-fonts-emoji
      # Other fonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      fantasque-sans-mono
      fira-code
      font-awesome-ttf
      google-fonts
      hack-font
      hasklig
      nerdfonts
      jetbrains-mono
      iosevka
      powerline-fonts
      material-icons
    ];
  };
}
