recrank:
	@echo -e "\033[0;32mBUILD\033[0m"
	@sudo nixos-rebuild build --flake .#rostam
	@echo -e "\033[0;32mSWITCH\033[0m"
	@sudo nixos-rebuild switch --flake .#rostam

upgrade:
	nix flake update
	sudo nixos-rebuild switch --flake .#rostam

clean:
	sudo nix-collect-garbage --delete-older-than 7d
