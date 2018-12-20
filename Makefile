all:	nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild switch

upgrade:
	sudo nix-channel --update
	sudo nixos-rebuild switch --upgrade

nocache:
	sudo nixos-rebuild switch --option build-use-substitutes false

stow:
	stow -v 1 xmonad i3

stow_osx:
	stow -v 1 nix-darwin

# https://github.com/NixOS/nixpkgs/issues/23926
workaround:
	sudo nix-collect-garbage --delete-older-than 60d

check:
	curl -s http://howoldis.herokuapp.com/api/channels | gron | grep "json\[0\]" | gron --ungron | grep --color=always -E "(name|humantime)"

