all:	stow nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild switch

upgrade:
	sudo nixos-rebuild switch --upgrade

nocache:
	sudo nixos-rebuild switch --option build-use-substitutes false

stow:
	stow -v 1 bash git emacs xmonad i3
