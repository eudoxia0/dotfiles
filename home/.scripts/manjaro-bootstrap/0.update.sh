#!/bin/sh

# Prepare pacman

sudo pacman -S manjaro-system pacman
sudo pacman -S archlinux-keyring manjaro-keyring
sudo pacman-key --refresh-keys
sudo pacman-mirrors -g
sudo pacman-optimize

# Update

sudo pacman -Syu

# At this point the script will probably fail, and you have to fix the updates
# yourself
