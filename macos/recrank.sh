#!/usr/bin/env zsh

cp config/zprofile.sh ~/.zprofile
cp config/zshrc.sh ~/.zshrc
cp config/gitconfig ~/.gitconfig
mkdir -p ~/.local/bin
cp -a scripts/* ~/.local/bin/
