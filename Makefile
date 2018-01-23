all:
	sudo nixos-rebuild switch

stow:
	stow -v 2 bash git emacs
