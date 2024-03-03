#!/usr/bin/env zsh

cp config/zprofile.sh ~/.zprofile
cp config/zshrc.sh ~/.zshrc
cp config/gitconfig.conf ~/.gitconfig
cp config/gitignore.txt ~/.gitignore
mkdir -p ~/.local/bin
cp -a scripts/* ~/.local/bin/
touch ~/.hushlogin
