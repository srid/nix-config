all:	switch
	@echo

switch:
	sudo nixos-rebuild switch -j auto

switch-remote:
	sudo nixos-rebuild switch -j0

switch-local:
	sudo nixos-rebuild switch -j auto --builders ""

update-deps:
	nix-thunk update dep/home-manager

upgrade:	update-deps
	sudo nixos-rebuild switch -j auto --upgrade

upgrade-remote:	update-deps
	sudo nixos-rebuild switch --upgrade -j0

upgrade-local:	update-deps
	sudo nixos-rebuild switch -j auto --upgrade --builders ""

# Use this if one of the cache is down
nocache:
	sudo nixos-rebuild switch --option build-use-substitutes false

# Use this only to freeup disk space (at the cost losing cached builds, esp. re: ghcjs).
# To free up space in /boot, use `make freeupboot`
# workaround:
#	sudo nix-collect-garbage --delete-older-than 7d

freeupboot:
	# Delete all but the last two generations
	sudo nix-env -p /nix/var/nix/profiles/system --delete-generations +5
	sudo nixos-rebuild boot
