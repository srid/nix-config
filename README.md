# My Nix configuration

<!-- vim-markdown-toc GFM -->

* [On NixOS](#on-nixos)
* [On macOS & Other Linux](#on-macos--other-linux)
* [Hardware notes](#hardware-notes)
  * [Thinkpad P71](#thinkpad-p71)
    * [Preparation](#preparation)
    * [Install](#install)
  * [Kimsufi](#kimsufi)
  * [Hyper-v](#hyper-v)
* [Tips and tricks](#tips-and-tricks)
  * [Resizing VM's disk](#resizing-vms-disk)

<!-- vim-markdown-toc -->

## On NixOS

1. First, copy the 'srid' user config to configuration.nix, and activate that nix. We need to do 
   this before cloning the repo under srid's home directory. 

1. Then, ssh as srid@... and:

```
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
sudo nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
sudo nix-channel --update

ssh-keygen  # then, add to Github

nix-shell -p git -p vim
...
git clone git@github.com:srid/nix-config.git $HOME/nix-config
cd $HOME/nix-config 

# First, review ./machine/???.nix
sudo mv /etc/nixos/configuration.nix /tmp/
sudo ln -s $(pwd)/machine/???.nix /etc/nixos/configuration.nix
make
```

## On macOS & Other Linux

Use this method on macOS and other Linux distros including ChromeOS's crostini container.

1. Install [home-manager](https://github.com/rycee/home-manager)
1. `ln -s ~/nix-config/nix/home.nix ~/.config/nixpkgs/home.nix`
1. `mkdir old-profile; mv .bashrc .profile old-profile`
1. `home-manager switch`

## Hardware notes

### Thinkpad P71

#### Preparation
- In BIOS, disable discrete graphics so intel card is used. We can change this post-install.
- In BIOS, make the USB disk the highest startup priority
- Plug the Ethernet cable for direct internet (wifi will be configured post-install)

#### Install
- Follow the NixOS installation manual (choose UEFI), using /dev/nvmen1 disk)
- Boot into NixOS and follow the instructions in [nix-config](https://github.com/srid/nix-config) to complete the configuration.
- Unplug the Enternet cable, and test WIFI using `nmtui`

### Kimsufi

Use https://github.com/jeaye/nixos-in-place on top of Ubuntu.

Prior to running install.sh, patch the configuration.nix used by it to allow root logins; see https://github.com/jeaye/nixos-in-place/issues/43

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
