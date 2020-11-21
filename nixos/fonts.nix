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

      # Doom emacs
      emacs-all-the-icons-fonts
    ];
  };
}
