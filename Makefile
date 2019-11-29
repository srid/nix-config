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
	cd dotfiles && stow -v 1 -t ${HOME} *

# https://github.com/NixOS/nixpkgs/issues/23926
workaround:
	sudo nix-collect-garbage --delete-older-than 7d
