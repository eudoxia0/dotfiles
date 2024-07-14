#!/bin/sh
#
# Usage:
#
#   ./backup.sh <disk name>

rsync --progress \
      --archive \
      --human-readable \
      --perms \
      --executability \
      --times \
      --checksum \
      --delete \
      ~/Root/ \
      /Volumes/$1/Root
