#!/bin/sh

# backup - copy important files to USB drive, Dropbox
# usage: backup

if [ "$HOSTNAME" = desktop ]; then
    base=/run/media/eudoxia
else
    base=/media/eudoxia
fi

unison ~/code $base/backup/code
unison ~/images $base/backup/images
unison ~/writing $base/backup/writing
unison ~/self $base/backup/self
unison ~/.scripts $base/backup/.scripts
