#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "Invalid number of arguments"
    exit
fi

BACKUP_DRIVE=/media/eudoxia/$1

if [ ! -d $BACKUP_DRIVE ]; then
    echo "Backup directory does not exist."
    exit
fi

rsync --progress --archive --human-readable --perms --executability --times --checksum --delete ~/files/ $BACKUP_DRIVE/files
