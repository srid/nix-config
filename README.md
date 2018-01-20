# NixOS dotfiles

My NixOS configuration

## Setting up a new machine

1. [Add an user](https://nixos.org/nixos/manual/index.html#sec-user-management) to `configuration.nix` and ssh as that user

1. `nix-shell -p git` and clone this repo under `$HOME/mynixos` (not anywhere else)

1. Include the `default.nix` file from `configuration.nix`

1. Install nix packages using `make`

1. Stow dotfiles using `make stow`

1. Install spacemacs (`git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d`) and launch Emacs

