{ config, pkgs, ... }:

{
  imports = 
    [ "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nix-darwin"
    ];

  users.users.srid = { };
  home-manager.users.srid = (import ../mynixos/nix/home.nix);

  # TODO: Share with nix/*.nix
  environment.systemPackages = with pkgs;
    [ stow 
      emacs
      fzf
      silver-searcher
      mosh
      haskellPackages.stylish-haskell
      (callPackage (import /Users/srid/mynixos/nix/nvim/default.nix) {})
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 3;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 8;
  nix.buildCores = 8;
}
