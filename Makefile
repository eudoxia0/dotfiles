HOSTNAME := $(shell hostname)

recrank:
	nh os switch .#$(HOSTNAME)

home:
	nh home switch

upgrade:
	nix flake update

clean:
	nh clean all --keep 5
