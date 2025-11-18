recrank:
	nh os switch .

upgrade:
	nix flake update

clean:
	nh clean all --keep 5
