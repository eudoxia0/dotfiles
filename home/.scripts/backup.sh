#!/bin/sh

# backup - copy important files to USB drive
# usage: backup.sh

if [ "$HOSTNAME" = desktop ]; then
    base=/run/media/eudoxia # This is Arch
else
    base=/media/eudoxia # This is Ubuntu
fi

unison ~/code $base/backup/code
unison ~/images $base/backup/images
unison ~/writing $base/backup/writing
unison ~/self $base/backup/self
unison ~/.scripts $base/backup/.scripts
unison ~/backup $base/backup/backup
