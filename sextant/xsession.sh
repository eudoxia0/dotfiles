xmodmap ~/.Xmodmap
xrdb ~/.Xresources
xcape -e "Shift_L=parenleft;Shift_R=parenright"
xscreensaver -no-splash &
.scripts/battery.sh &
xsetroot -solid "gray40"

exec fvwm
