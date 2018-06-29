# NixOS dotfiles

My NixOS configuration

## Setting up a new machine

1. First, copy the 'srid' user config to configuration.nix, and activate that nix. We need to do this before cloning the repo under srid's home directory. 

1. Then, ssh as srid@... (make sure port forwarding is enabled, if possible, for git clone), and:

```
nix-shell -p git -p emacs -p stow
...
git clone git@github.com:srid/mynixos.git $HOME/mynixos
cd $HOME/mynixos 
sudo ln -s $(pwd)/configuration-tinix.nix` /etc/nixos/configuration.nix  # review original first
make
```
