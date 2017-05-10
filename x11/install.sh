#!/usr/bin/env bash

SCREEN_DIR=~/.screenlayout

if [ ! -d "$SCREEN_DIR" ]; then
  mkdir $SCREEN_DIR
fi

cp xsession.sh ~/.xsession
cp xresources ~/.Xresources
cp xscreensaver ~/.xscreensaver
cp redshift.conf ~/.config/redshift.conf
cp gtkrc.conf ~/.gtkrc-2.0
cp compose.txt ~/.XCompose
cp modmap.txt ~/.Xmodmap

# Copy XDM-related configuration

sudo cp xdm/Xsetup /etc/X11/xdm/Xsetup
sudo cp xdm/Xstartup /etc/X11/xdm/Xstartup

# Copy the wallpapers

WALLPAPERS_DIR=~/.walls

mkdir -p $WALLPAPERS_DIR
cp -R ../wallpapers/. $WALLPAPERS_DIR