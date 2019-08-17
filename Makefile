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
	cd dotfiles && stow -v 1 *

stow_osx:
	cd dotfiles.osx && stow -v 1 -t ${HOME} *

# https://github.com/NixOS/nixpkgs/issues/23926
workaround:
	sudo nix-collect-garbage --delete-older-than 7d

check:
	curl -s http://howoldis.herokuapp.com/api/channels | gron | grep "json\[0\]" | gron --ungron | grep --color=always -E "(name|humantime)"

