#!/bin/sh

dbus-update-activation-environment --systemd DISPLAY XAUTHORITY

exec stumpwm
