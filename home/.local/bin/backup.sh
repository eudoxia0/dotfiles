#!/bin/sh
#
# Usage:
#
#   ./backup.sh <disk name>

rsync --progress \
      --archive \
      --human-readable \
      --checksum \
      --delete \
      ~/Root/ \
      /Volumes/$1/Root
