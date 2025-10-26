#!/usr/bin/env bash

sudo nix-channel --update
sudo nixos-rebuild switch -I nixos-config=configuration.nix --upgrade
