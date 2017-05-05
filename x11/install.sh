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

WALLPAPERS_DIR=~/.walls

mkdir -p $WALLPAPERS_DIR
cp -R ../wallpapers/. $WALLPAPERS_DIR