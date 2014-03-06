#!/bin/sh

# Install drivers
# Recommend do this outside of X

sudo mhwd -a pci nonfree 0300
sudo mhwd-gpu --setgl nvidia

sudo reboot

# Install packages

sudo pacman -S yaourt rxvt-unicode git firefox bitcoin-qt sqlite3 nss feh \
  redshift transmission-gtk make automake emacs unison pcmanfm giflib gimp \
  gimp inkscape terminus-font autoconf libtool autogen

# Various other tools
sudo pacman -S clang llvm keepass maxima gnuplot patch meld scrot \
  calibre tint2 banshee cmake bzr vala python2-pip screenfetch \
  numlockx scala sbt

# Non-tools
sudo yaourt ttf-win7-fonts

sudo yaourt tor-browser-en

# le bitbutts
sudo yaourt dogecoin-qt
sudo yaourt litecoin-qt
sudo yaourt electrum
sudo yaourt cpuminer

sudo yaourt marlin
sudo yaourt pandoc-static

# Set up environments for other languages

# Lisp

sudo pacman -S sbcl ecl clisp

# set up Quicklisp
curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-userinit --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
sbcl --no-userinit --load quicklisp/setup.lisp --eval "(ql:quickload :swank)" --quit
# we install swank here because it will be loaded by the init.lisp from my dotfiles
rm quicklisp.lisp

# Node

sudo pacman -S nodejs
sudo npm -g install jslint

# Ruby

# install rbenv and ruby-build plugin
sudo yaourt -S --noconfirm rbenv ruby-build rbenv-default-gems

# install a reasonably recent Ruby version
rbenv install 2.1.0

rbenv global 2.1.0

# install useful ruby gems


# Build stumpwm

cd Downloads/
git clone https://github.com/sabetts/stumpwm.git
cd stumpwm/
autoconf
./configure
sbcl --eval "(ql:quickload '(:clx :cl-ppcre :xembed))" --quit
make
sudo make install

# Set up homeshick
sudo yaourt -s --noconfirm homeshick-git
homeshick clone eudoxia0/dotfiles

# Remove packages

sudo pacman -Rns conky nitrogen parcellite synapse geany lxterminal tint2 \
  obconf tintwizard qupzilla openbox obmenu-generator openbox-themes \
  lxappearance-obconf thunar thunar-archive-plugin thunar-volman xnoise
