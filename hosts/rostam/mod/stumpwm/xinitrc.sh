#!/bin/sh

polybar &
feh --no-fehbg --bg-fill ~/.local/share/eudoxia/panther.jpg

exec stumpwm
