#!/usr/bin/env bash

# Node

sudo npm -g install jslint

# Ruby

rbenv install 2.1.0 -k
rbenv global 2.1.0

# install useful ruby gems
gem install veewee
gem install sass

# Lisp
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
  --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

# Turn this thing up
alsamixer

# Create directories, remove some default ones
rm -rf Documents Manjaro Music Pictures Public Templates Videos
mkdir torrents
mkdir .session
