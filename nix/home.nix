# https://nixos.wiki/wiki/Home_Manager

# Stuff on this file should work across all of my computing devices. 
# Presently these are: Thinkpad, Macbook and Pixel Slate.

{ config, pkgs, ...}:

{
  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  # Utility functions
  _module.args = {
    fetchGH = fq: rev: builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  };

  imports = [
    ./home/shells.nix
    ./home/git.nix
    ./home/gpg.nix
    ./home/tmux.nix
    ./home/keybase.nix
    ./home/haskell.nix
    ./home/gotty.nix
  ];

  home.packages = with pkgs; [
    (callPackage ./nvim.nix {})
    awscli
    dejavu_fonts
    source-serif-pro
    aria
    cachix
    htop
    file
    fortune
    gron
    mosh
    mpv
    ripgrep
    # sshfs -- TODO: not available on darwin
    tig
    transmission
    youtube-dl
    pandoc
  ];

  home.sessionVariables = {
    # https://github.com/syl20bnr/spacemacs/wiki/Terminal
    # TERM = "xterm-24bit";  breaks on crostini
    EDITOR = "nvim";
  };

  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 128;
  };


  # TODO: Don't enable these on crostini

  # Automounter for removable media.
  services.udiskie = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    automount = true;
    notify = true;
  };

  services.redshift = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    tray = true;
    # Quebec City
    latitude = "46.8423";
    longitude = "-71.2429";
  };

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
