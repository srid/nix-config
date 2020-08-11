all:	nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild -j auto switch

upgrade:
	sudo nix-channel --update
	sudo nixos-rebuild -j auto switch --upgrade

# Use this if one of the cache gets broken
nocache:
	sudo nixos-rebuild -j auto switch --option build-use-substitutes false

# https://github.com/NixOS/nixpkgs/issues/23926
workaround:
	sudo nix-collect-garbage --delete-older-than 7d

