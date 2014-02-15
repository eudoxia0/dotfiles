#!/bin/sh

# Install packages

sudo pacman -S yaourt rxvt-unicode git firefox bitcoin-qt sqlite3 nss feh \
  redshift transmission-gtk make automake emacs unison pcmanfm giflib gimp \
  sbcl ecl clisp gimp inkscape terminus-font autoconf

# Set up Quicklisp

curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
sbcl --load quicklisp/setup.lisp --eval "(ql:quickload :swank)" --quit

# Set up homeshick
git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
printf '\nsource "$HOME/.homesick/repos/homeshick/homeshick.sh"' >> $HOME/.bashrc
source $HOME/.bashrc

# Set up ssh

ssh-keygen -t rsa -C "eudoxiahp@gmail.com"
cat .ssh/id_rsa.pub

# Wait for me to put my key in github
read -n1 -r -p "[Press any key to continue]" key

homeshick clone eudoxia0/dotfiles

# Build stumpwm

cd Downloads/
git clone https://github.com/sabetts/stumpwm.git
cd stumpwm/
autoconf
./configure
sbcl --eval "(ql:quickload '(:clx :cl-ppcre :xembed))" --quit
make
sudo make install
