# My NixOS configuration

## Setup

1. First, copy the 'srid' user config to `device`, and activate that. We need to do this before cloning the repo under srid's home directory.

1. Then, ssh as srid@... and:

```bash
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
sudo nix-channel --update

ssh-keygen  # then, add to Github

nix-shell -p git -p vim
...
git clone git@github.com:srid/nix-config.git $HOME/nix-config
cd $HOME/nix-config 

# First, review ./hardware/???.nix
sudo mv /etc/nixos/hardware /tmp/
sudo ln -s $(pwd)/hardware/???.nix /etc/nixos/configuration.nix
make
```

## Installing NixOS on ...

### Thinkpad X1C7

https://notes.srid.ca/x1c7-install

### Thinkpad P71

- In BIOS, disable discrete graphics so intel card is used. We can change this post-install.
- In BIOS, make the USB disk the highest startup priority
- Follow the NixOS installation manual (choose UEFI), using /dev/nvme0n1 disk

### Digital Ocean

Use [nixos-infect](https://github.com/elitak/nixos-infect).
