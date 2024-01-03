#!/usr/bin/env bash

hostname=`hostname`

function recrank() {
    sudo nixos-rebuild switch -I nixos-config=$1.nix
}

if [ "$hostname" == "sextant" ]; then
    recrank sextant
elif [ "$hostname" == "bullroarer" ]; then
    recrank bullroarer
else
    if [ -z "$1" ]; then
        echo "unknown hostname"
	    exit 1
    else
	    recrank $1
    fi
fi
