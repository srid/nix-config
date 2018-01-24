all:	stow nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild switch

stow:
	stow -v 1 bash git emacs
