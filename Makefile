recrank:
	nh os switch .

home:
	nh home switch

upgrade:
	nix flake update

clean:
	nh clean all --keep 5
