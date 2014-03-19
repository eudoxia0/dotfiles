#!/bin/bash

# backup - copy important files to USB drive
# usage: backup.sh

if [ "$HOSTNAME" = desktop ]; then
    base=/run/media/eudoxia/backup # This is Arch
else
    base=/media/eudoxia/backup # This is Ubuntu
fi

if [ -d "$base/Kindle" ]; then
  # Is the Kindle on?
  cp "$base/Kindle/documents/My Clippings.txt" ~/self/clippings.txt
fi

sync () {
  # Function to sync directories, optionally passing extra args
  echo -e "\e[31m$1\e[0m"
  unison ~/$1 $base/$1 ${@:2}
}

sync code
sync images
sync writing
sync self
sync .scripts
sync backup
sync notes
sync library
sync .newsbeuter/urls
sync .newsbeuter/config
sync .ssh -ignore "Name known_hosts"
sync .irclogs

chown -R $USER ~/.ssh
