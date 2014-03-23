#!/bin/sh

# Install packages

sudo pacman -S yaourt rxvt-unicode git firefox bitcoin-qt sqlite3 nss feh \
  redshift rtorrent make automake emacs unison pcmanfm giflib gimp gimp \
  inkscape terminus-font autoconf libtool autogen clang llvm keepass maxima \
  gnuplot patch meld scrot calibre tint2 banshee cmake bzr vala python2-pip \
  screenfetch numlockx scala sbt ruby bison graphviz ditaa virtualbox sbcl \
  ecl clisp nodejs units pcmanfm ansible newsbeuter

# Yaourt packages
sudo yaourt -S --noconfirm ttf-win7-fonts-autodownload tor-browser-en \
  dogecoin-qt litecoin-qt electrum cpuminer pandoc-static rbenv ruby-build \
  vagrant-git rbenv-default-gems

# Set up environments for other languages

# Lisp

# set up Quicklisp
curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-userinit --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
sbcl --no-userinit --load quicklisp/setup.lisp --eval "(ql:quickload :swank)" --quit
# we install swank here because it will be loaded by the init.lisp from my dotfiles
rm quicklisp.lisp

# Node

sudo npm -g install jslint

# Ruby 

# install a reasonably recent Ruby version
rbenv install 2.2.0-dev
rbenv install 2.1.0
rbenv install 1.9.3-dev

rbenv global 2.1.0

# install useful ruby gems
gem install veewee

# Set up homeshick
sudo yaourt -s --noconfirm homeshick-git
homeshick clone eudoxia0/dotfiles

# Make sure the USB drive is mounted
backup.sh

# Now that we have init.lisp:
sbcl --eval "(ql:quickload :quicklisp-slime-helper)" --quit

# Build stumpwm

cd Downloads/
git clone https://github.com/sabetts/stumpwm.git
cd stumpwm/
autoconf
./configure
sbcl --eval "(ql:quickload '(:clx :clx-truetype :cl-ppcre :xembed))" --quit
make
sudo make install
