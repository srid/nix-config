# linuxvm

My Linux VM configuration

## Setting up a new machine

1. [Add an user](https://nixos.org/nixos/manual/index.html#sec-user-management) to `configuration.nix` and ssh as that user

1. `nix-shell -p git` and clone this repo

1. Include the `default.nix` file from `configuration.nix`

1. Run `sudo nixos-rebuild switch`

1. TODO: GNU stow?

1. TODO: Git config
```
  git config --global user.email "you@example.com"
  git config --global user.name "Your Name"
```
