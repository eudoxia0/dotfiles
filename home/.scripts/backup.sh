#!/bin/sh

# backup - copy important files to USB drive
# usage: backup.sh

if [ "$HOSTNAME" = desktop ]; then
    base=/run/media/eudoxia # This is Arch
else
    base=/media/eudoxia # This is Ubuntu
fi

if [ -d "$base/Kindle" ]; then
  # Is the Kindle on?
  cp "$base/Kindle/documents/My Clippings.txt" ~/self/clippings.txt
fi

unison ~/code $base/backup/code
unison ~/images $base/backup/images
unison ~/writing $base/backup/writing
unison ~/self $base/backup/self
unison ~/.scripts $base/backup/.scripts
unison ~/backup $base/backup/backup
unison ~/notes $base/backup/notes
unison ~/library $base/backup/library
unison ~/.shell/hosts.txt $base/backup/.shell/hosts.txt
unison ~/.config/transmission $base/backup/.config/transmission
unison ~/.localrc $base/backup/.localrc
unison ~/.liferea_1.8 $base/backup/liferea_1.8

