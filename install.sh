#!/usr/bin/env bash

function set_up () {
  echo -e "\e[31mSetting up $1...\e[0m"
  cd $2
  bash install.sh
  cd ..
}

set_up "Common Lisp" lisp
set_up "Emacs" emacs
set_up "shell" shell
set_up "X11" x11
set_up "wm" wm
set_up "scripts" scripts
set_up "other configuration" config
