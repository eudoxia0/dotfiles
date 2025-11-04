HOSTNAME := $(shell hostname)

recrank:
	sudo nixos-rebuild switch --flake .#$(HOSTNAME)

upgrade:
	nix flake update
	sudo nixos-rebuild switch --flake .#$(HOSTNAME)

clean:
	sudo nix-collect-garbage --delete-older-than 7d
