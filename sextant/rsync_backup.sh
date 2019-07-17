#!/usr/bin/env bash

set -euxo pipefail
echo "Drive: (probably /media/eudoxia/BACKUP)"
read BACKUP_DRIVE

rsync --quiet --archive --human-readable --perms --executability --times --checksum --delete \
      ~/files/ \
      $BACKUP_DRIVE/files
