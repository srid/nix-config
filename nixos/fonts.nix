{ config, lib, pkgs, ... }:

{
   fonts = {
    enableDefaultFonts = true;

    # NOTE: Some fonts may break colour emojis in Chrome
    # cf. https://github.com/NixOS/nixpkgs/issues/69073#issuecomment-621982371
    fonts = with pkgs; [
      noto-fonts-emoji

      # For fish powerline plugin
      powerline-fonts
      cascadia-code

      # Keep this disabled until verifying they don't break emoji font
      #emacs-all-the-icons-fonts
      #fantasque-sans-mono
      #fira-code
      #font-awesome-ttf
      #google-fonts
      #hack-font
      #hasklig
      #nerdfonts
      #iosevka
      #powerline-fonts
      #material-icons
    ];
  };
}
