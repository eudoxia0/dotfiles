#!/bin/bash

cd ~/torrents/watch
#aria2c --bt-metadata-only=true --bt-save-metadata=true $1 > /dev/null

[[ "$1" =~ xt=urn:btih:([^&/]+) ]] || exit;
echo "d10:magnet-uri${#1}:${1}e" > "meta-${BASH_REMATCH[1]}.torrent"
