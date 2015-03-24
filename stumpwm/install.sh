#!/usr/bin/env bash

cp rc.lisp ~/.stumpwmrc

STUMP_DIR=~/.stumpwm

if [ -d "$STUMP_DIR" ]; then
  rm -rf $STUMP_DIR
fi

mkdir $STUMP_DIR

for file in option commands keys gaps alarm
do
  cp $file.lisp $STUMP_DIR/$file.lisp
done
