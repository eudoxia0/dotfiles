#!/bin/sh
#
# Usage:
#
#   ./backup.sh <disk name>

rsync --progress \
      --archive \
      --human-readable \
      --delete \
      /home/eudoxia/root/ \
      /run/media/eudoxia/$1/root
