#!/bin/sh

# Install packages

sudo pacman -S yaourt rxvt-unicode git firefox bitcoin-qt sqlite3 nss feh \
  redshift transmission-gtk make automake emacs unison pcmanfm giflib gimp \
  inkscape terminus-font autoconf libtool autogen clang llvm keepass maxima \
  gnuplot patch meld scrot calibre tint2 banshee cmake bzr vala python2-pip \
  screenfetch numlockx scala sbt ruby bison graphviz ditaa virtualbox nodejs \
  units pcmanfm ansible newsbeuter bash-completion skype python2-virtualenv \
  python-virtualenvwrapper python2-pylint r net-tools virtualbox-host-modules \
  zathura zathura-pdf-mupdf zathura-djvu xmonad xmonad-contrib gmrun xcompmgr \
  nfs-utils gdb valgrind sbcl hunspell hunspell-en python-sphinx erlang

# Yaourt packages
sudo yaourt -S --noconfirm ttf-win7-fonts-autodownload tor-browser-en \
  dogecoin-qt litecoin-qt electrum cpuminer pandoc-static rbenv ruby-build \
  vagrant-git rbenv-default-gems heroku-toolbelt ttf-monaco grive tsung \
  libfixposix

# Set up homeshick
sudo yaourt -s --noconfirm homeshick-git
homeshick clone eudoxia0/dotfiles

# Set up environments for other languages

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

# Lisp
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
  --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

sbcl_gencore
sudo mv sbcl.core /usr/lib/sbcl/sbcl.core

# Make sure the USB drive is mounted
backup.sh

# Setup NFS services (For Vagrant)
sudo systemctl enable rpc-idmapd
sudo systemctl enable rpc-mountd
