all:	nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild switch

upgrade:
	sudo nix-channel --update
	sudo nixos-rebuild switch --upgrade

nocache:
	sudo nixos-rebuild switch --option build-use-substitutes false

# https://github.com/NixOS/nixpkgs/issues/23926
workaround:
	sudo nix-collect-garbage --delete-older-than 7d

# TODO: Do it in nix
terminfo:
	tic -x -o ~/.terminfo terminfo-24bit.src
