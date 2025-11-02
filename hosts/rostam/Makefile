recrank:
	sudo nixos-rebuild switch --flake .#rostam

upgrade:
	nix flake update
	sudo nixos-rebuild switch --flake .#rostam

clean-nix:
	sudo nix-collect-garbage --delete-older-than 7d
