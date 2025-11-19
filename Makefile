recrank:
	sudo nixos-rebuild switch --flake .#$(shell hostname)

upgrade:
	nix flake update

clean:
	sudo nix-collect-garbage --delete-older-than 7d
