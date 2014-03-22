#!/bin/sh

# Prepare pacman

sudo pacman -S manjaro-system pacman
sudo pacman -S archlinux-keyring manjaro-keyring
sudo pacman-key --refresh-keys
sudo pacman-mirrors -g
sudo pacman-optimize

# I'm going to remove these anyways, so let's speed up the update

sudo pacman -Rns conky nitrogen parcellite synapse geany obconf tintwizard \
  qupzilla openbox obmenu-generator openbox-themes lxappearance-obconf thunar \
  thunar-archive-plugin thunar-volman xnoise

# Update

sudo pacman -Syu

# At this point the script will probably fail, and you have to fix the updates
# yourself
