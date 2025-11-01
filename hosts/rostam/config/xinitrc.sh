#!/bin/sh

# eval $(gnome-keyring-daemon --start --components=secrets,ssh)
# export $(gnome-keyring-daemon --start --components=secrets,ssh)
# dbus-update-activation-environment --systemd DISPLAY

exec stumpwm
