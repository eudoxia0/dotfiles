#!/usr/bin/env bash

DOSBOX_DIR=~/.dosbox
RSS_DIR=~/.newsbeuter

if [ ! -d "$DOSBOX_DIR" ]; then
  mkdir $DOSBOX_DIR
fi

if [ ! -d "$RSS_DIR" ]; then
  mkdir $RSS_DIR
fi

cp dosbox.conf ~/.dosbox/dosbox-0.74.conf
cp newsbeuter.conf ~/.newsbeuter/config
cp ocamlinit.ml ~/.ocamlinit
cp psqlrc ~/.psqlrc
cp gitconfig ~/.gitconfig
cp gargoylerc ~/.garglkrc
