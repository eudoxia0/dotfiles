#!/usr/bin/env bash

sudo nix-channel --update
sudo nixos-rebuild switch -I nixos-config=rostam.nix --upgrade
