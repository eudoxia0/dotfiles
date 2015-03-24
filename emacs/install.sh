#!/usr/bin/env bash

EMACS_DIR=~/.emacs.d

if [ -d "$EMACS_DIR" ]; then
  rm -rf $EMACS_DIR
fi

mkdir $EMACS_DIR
mkdir $EMACS_DIR/lang

for file in init config file-modes keys packages theme lang/lisp \
            lang/haskell lang/lass
do
  cp $file.el $EMACS_DIR/$file.el
done
