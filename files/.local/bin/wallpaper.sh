#!/usr/bin/env bash

if [ -d ~/files/Images/Wallpapers/ ]; then
    # If the wallpapers directory exists, choose a random wallpaper and switch
    # every 5 minutes.
    feh --randomize --bg-fill ~/files/Images/Wallpapers/rotation/*
    sleep 300
else
    # Otherwise set a default colour.
    xsetroot -solid '#555577'
fi
