#!/bin/sh
# irssi-scriptman: download an irssi script if it's not already there
# usage: irssi-scriptman.sh <script name> <script url>

BASE=~/.irssi/scripts
SCRIPT_PATH=$BASE/$1.pl
AUTORUN_PATH=$BASE/autorun/$1.pl

mkdir -p $BASE/autorun

if [ -f $SCRIPT_PATH ]; then
  echo "Script found. Skipping"
else
  curl $2 -o $SCRIPT_PATH
  ln -s $SCRIPT_PATH $AUTORUN_PATH;
fi
