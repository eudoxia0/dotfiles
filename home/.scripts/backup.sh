#!/bin/sh

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

unison ~/code $base/code
unison ~/images $base/images
unison ~/writing $base/writing
unison ~/self $base/self
unison ~/.scripts $base/.scripts
unison ~/backup $base/backup
unison ~/notes $base/notes
unison ~/library $base/library
unison ~/.shell/hosts.txt $base/.shell/hosts.txt
unison ~/.config/transmission $base/.config/transmission
unison ~/.localrc $base/.localrc
unison ~/.liferea_1.8 $base/.liferea_1.8
unison ~/.ssh $base/.ssh

