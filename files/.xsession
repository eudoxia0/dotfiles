wallpaper.sh &
xsetroot -solid '#708090'
xmodmap ~/.config/x11/xmodmap &
xrdb ~/.config/x11/xresources &
xcape -e "Shift_L=parenleft;Shift_R=parenright" &
xscreensaver -no-splash &
if [ -n $(which "$SCRIPT_NAME") ]; then
    battery.sh &
fi
exec spectrwm
