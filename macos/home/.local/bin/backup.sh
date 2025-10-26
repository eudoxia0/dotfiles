#!/bin/sh
#
# Usage:
#
#   ./backup.sh <disk name>

rsync --progress \
      --archive \
      --human-readable \
      --delete \
      /Volumes/Asterion/Root/ \
      /Volumes/$1/Root
