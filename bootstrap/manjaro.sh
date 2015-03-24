#!/usr/bin/env bash

# Prepare pacman

sudo pacman -S manjaro-system pacman
sudo pacman -S archlinux-keyring manjaro-keyring
sudo pacman-key --refresh-keys
sudo pacman-mirrors -g
sudo pacman-optimize

# I'm going to remove these anyways, so let's speed up the update

sudo pacman -Rns conky nitrogen parcellite synapse geany obconf tintwizard \
  qupzilla openbox obmenu-generator openbox-themes lxappearance-obconf thunar \
  thunar-archive-plugin thunar-volman xnoise evince

# Update

sudo pacman -Syu

# At this point the script will probably fail, and you have to fix the updates
# yourself

# Install drivers
# Recommend do this outside of X

sudo mhwd -a pci nonfree 0300
sudo mhwd-gpu --setgl nvidia

sudo reboot

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
  texlive-publishers texlive-latexextra cheese ocaml xdotool pep8

# Yaourt packages
sudo yaourt -S --noconfirm ttf-win7-fonts-autodownload tor-browser-en \
  electrum pandoc-static rbenv ruby-build vagrant-git rbenv-default-gems \
  heroku-toolbelt ttf-monaco grive tsung libfixposix wrk ccl-bin saxon-b \
  faba-icons-git moka-icons-git orchis-gtk-git xcape-git pidgin-opensteamworks

# Setup NFS services (For Vagrant)
sudo systemctl enable rpc-idmapd
sudo systemctl enable rpc-mountd
