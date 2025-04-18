#!/bin/sh
#
# Usage:
#
#   ./backup.sh <disk name>

rsync --progress \
      --archive \
      --human-readable \
      --executability \
      --checksum \
      --delete \
      ~/Root/ \
      /Volumes/$1/Root
