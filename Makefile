all:	nix-switch
	@echo

nix-switch:
	sudo nixos-rebuild switch

upgrade:
	sudo nix-channel --update
	sudo nixos-rebuild switch --upgrade

# Use this if one of the cache is down
nocache:
	sudo nixos-rebuild switch --option build-use-substitutes false

# Use this only to freeup disk space (at the cost losing cached builds, esp. re: ghcjs).
# To free up space in /boot, use `make freeupboot`
# workaround:
#	sudo nix-collect-garbage --delete-older-than 7d

freeupboot:
	# Delete all but the last two generations
	sudo nix-env -p /nix/var/nix/profiles/system --delete-generations +2
	sudo nixos-rebuild boot
