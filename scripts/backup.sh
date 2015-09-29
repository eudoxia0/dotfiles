#!/usr/bin/env bash

# backup - copy important files to USB drive
# usage: backup.sh

BASE=/media/eudoxia/backup

sync () {
  # Function to sync directory contents
  echo -n -e "\e[31m·\e[0m $1"
  rsync -a ~/$1/ $BASE/$1/
  echo -e " \e[32m✓\e[0m"
}

sync code
sync images
sync writing
sync self
sync backup
sync library
sync music
sync .newsbeuter
sync .ssh
sync .purple
sync .fonts
