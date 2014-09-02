#!/usr/bin/env bash

# Make sure the USB drive is mounted
backup.sh

# Set up environments for other languages
# Node

sudo npm -g install jslint

# Ruby 

# install a reasonably recent Ruby version
rbenv install 2.2.0-dev
rbenv install 2.1.0
rbenv install 1.9.3-dev

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

# Since we have a system SBCL, let's download it from the repo and build a
# recent local copy
git clone https://github.com/sbcl/sbcl.git
cd sbcl
sh make.sh
sudo sh install.sh

# Turn this thing up
alsamixer

# Create directories, remove some default ones
rm -rf Documents Manjaro Music Pictures Public Templates Videos
mkdir torrents
mkdir .session
