#!/usr/bin/env bash

# Install packages

sudo pacman -S yaourt rxvt-unicode git firefox bitcoin-qt sqlite3 nss feh \
  redshift transmission-gtk make automake emacs unison pcmanfm giflib gimp \
  inkscape terminus-font autoconf libtool autogen clang llvm keepass maxima \
  gnuplot patch meld scrot calibre tint2 cmake bzr vala python2-pip \
  screenfetch numlockx scala sbt ruby bison graphviz ditaa virtualbox nodejs \
  units pcmanfm ansible newsbeuter bash-completion skype python2-virtualenv \
  python-virtualenvwrapper python2-pylint r net-tools virtualbox-host-modules \
  zathura zathura-pdf-mupdf zathura-djvu xmonad xmonad-contrib gmrun xcompmgr \
  nfs-utils gdb valgrind hunspell hunspell-en python-sphinx erlang povray \
  adobe-source-sans-pro-fonts doxygen nimrod blender tidyhtml sbcl asciidoc \
  pidgin pidgin-otr irssi flashplugin vlc texinfo arandr texlive-core \
  texlive-publishers texlive-latexextra cheese

# Yaourt packages
sudo yaourt -S --noconfirm ttf-win7-fonts-autodownload tor-browser-en \
  electrum pandoc-static rbenv ruby-build vagrant-git rbenv-default-gems \
  heroku-toolbelt ttf-monaco grive tsung libfixposix wrk ccl-bin saxon-b \
  faba-icons-git moka-icons-git orchis-gtk-git

# Setup NFS services (For Vagrant)
sudo systemctl enable rpc-idmapd
sudo systemctl enable rpc-mountd
