#!/usr/bin/env bash

# Apply the current configuration.
sudo nixos-rebuild switch -I nixos-config=root.nix
