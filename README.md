# NixOS dotfiles

My NixOS configuration

## Setting up a new machine

1. First, copy the 'srid' user config to configuration.nix, and activate that nix. We need to do 
   this before cloning the repo under srid's home directory. 

1. Then, ssh as srid@... and:

```
sudo nix-channel --add http://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
sudo nix-channel --update

ssh-keygen  # then, add to Github

nix-shell -p git -p vim -p stow
...
git clone git@github.com:srid/mynixos.git $HOME/mynixos
cd $HOME/mynixos 

# First, review configuration-tinix.nix
sudo mv /etc/nixos/configuration.nix /tmp/
sudo ln -s $(pwd)/configuration-tinix.nix /etc/nixos/configuration.nix
make
```
