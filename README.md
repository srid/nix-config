# My NixOS configuration

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

## On OSX

```
make stow_osx
nix-env -f nix/nvim -i  # Installs customized neovim
```

## Installing NixOS on ...

### Kimsufi

Use https://github.com/jeaye/nixos-in-place on top of Ubuntu.

Priot to running install.sh, patch the configuration.nix used by it to allow root logins; see https://github.com/jeaye/nixos-in-place/issues/43

### Hyper-v

- [Quick Create](https://blogs.windows.com/buildingapps/2018/09/17/run-ubuntu-virtual-machines-made-even-easier-with-hyper-v-quick-create/) a Ubuntu VM
- Install a fresh copy of NixOS, overwriting Ubuntu.
  - Mount the existing UEFI partition as `/boot` (instead of creating a new partition)
- Add a 2nd network that is "internal", and use the IP address of that network to ssh the VM.

## Tips and tricks

### Resizing VM's disk

- Resize the disk using the VM tools
- Enable `boot.growPartition` and reboot
- Run `resize2fs` on the root volume to make use of the new disk space
