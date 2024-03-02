#!/usr/bin/env bash
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
      ~/Root/ \
      /Volumes/$1/Root
