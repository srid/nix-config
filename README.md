# My NixOS configuration

## Setup

1. First, copy the 'srid' user config to `configuration.nix`, and activate that. We need to do this before cloning the repo under srid's home directory.

1. Then, ssh as srid@... and:

```bash
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
sudo nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
sudo nix-channel --update

ssh-keygen  # then, add to Github

nix-shell -p git -p vim
...
git clone git@github.com:srid/nix-config.git $HOME/nix-config
cd $HOME/nix-config 

# First, review ./configuration.nix/???.nix
sudo mv /etc/nixos/configuration.nix /tmp/
sudo ln -s $(pwd)/configuration.nix/???.nix /etc/nixos/configuration.nix
make
```

## Installing NixOS on ...

### Thinkpad X1C7

<https://www.srid.ca/f65d38df.html>

### Thinkpad P71

- In BIOS, disable discrete graphics so intel card is used. We can change this post-install.
- In BIOS, make the USB disk the highest startup priority
- Follow the NixOS installation manual (choose UEFI), using /dev/nvme0n1 disk

### Digital Ocean

Use [nixos-infect](https://github.com/elitak/nixos-infect).

### Kimsufi

Use https://github.com/jeaye/nixos-in-place on top of Ubuntu.

Prior to running install.sh, patch the configuration.nix used by it to allow root logins; see https://github.com/jeaye/nixos-in-place/issues/43

### OVH

https://www.srid.ca/137ae172.html
