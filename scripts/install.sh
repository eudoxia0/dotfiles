#!/usr/bin/env bash

SCRIPTS_DIR=~/.scripts

if [ -d "$SCRIPTS_DIR" ]; then
  rm -rf $SCRIPTS_DIR
fi

mkdir $SCRIPTS_DIR

cp backup.sh $SCRIPTS_DIR/backup
cp cl.sh $SCRIPTS_DIR/cl
cp diceware.lisp $SCRIPTS_DIR/diceware
cp rotate-wallpaper.sh $SCRIPTS_DIR/rotate_wallpaper
cp git-merge-repo.sh $SCRIPTS_DIR/git-merge-repo