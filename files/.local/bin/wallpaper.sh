#!/usr/bin/env bash

if [ -d ~/files/4\ Resources/6\ Images/wallpapers/rotation ]; then
    # If the wallpapers directory exists, choose a random wallpaper and switch
    # every 5 minutes.
    while true; do
        feh --randomize --bg-fill ~/files/4\ Resources/6\ Images/wallpapers/rotation/*
        sleep 300
    done
else
    # Otherwise set a default colour.
    xsetroot -solid '#555577'
fi
