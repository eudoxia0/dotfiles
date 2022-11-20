#!/usr/bin/env bash

set -euxo pipefail
BACKUP_DRIVE=/media/eudoxia/Backup

rsync --progress --archive --human-readable --perms --executability --times --checksum --delete ~/files/ $BACKUP_DRIVE/files
