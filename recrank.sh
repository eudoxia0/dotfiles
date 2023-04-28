#!/usr/bin/env bash

# Ensure hardware-configuration.nix exists.
if [ ! -e "hardware-configuration.nix" ]; then
    sudo nixos-generate-config --dir .
fi

# Apply the current configuration.
sudo nixos-rebuild switch -I nixos-config=configuration.nix
