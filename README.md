# NixOS dotfiles

My NixOS configuration

## Setting up a new machine

1. `nix-shell -p git` and clone this repo under `$HOME/mynixos` (not anywhere else)

1. Symlink `configuration-???.nix` to `/etc/nixos/configuration.nix`

1. Stow dotfiles, and install nix packages using `make`

