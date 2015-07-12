#!/usr/bin/env bash

SCREEN_DIR=~/.screenlayout

if [ ! -d "$SCREEN_DIR" ]; then
  mkdir $SCREEN_DIR
fi

cp xsession.sh ~/.xsession
cp xresources ~/.Xresources
cp xscreensaver ~/.Xscreensaver
cp layout.sh ~/.screenlayout/default.sh
cp redshift.conf ~/.config/redshift.conf
cp gtkrc.conf ~/.gtkrc-2.0

chmod +x ~/.screenlayout/default.sh