#!/bin/sh

# Turn this thing up
alsamixer

# Create directories, remove some default ones
rm -rf Documents Manjaro Music Pictures Public Templates Videos
mkdir torrents

# Set up environments for other languages
sudo pacman -S nodejs
sudo npm -g install jslint

# Various other tools
sudo pacman -S clang llvm keepass maxima gnuplot patch meld

sudo yaourt tor-browser-en

# le bitbutts
sudo pacman -S bitcoin-qt
sudo yaourt dogecoin-qt
sudo yaourt litecoin-qt
sudo yaourt electrum
sudo yaourt cpuminer