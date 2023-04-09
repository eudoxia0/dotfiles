wallpaper.sh &
recorder.sh &
xmodmap ~/.config/x11/xmodmap &
xrdb ~/.config/x11/xresources &
xcape -e "Shift_L=parenleft;Shift_R=parenright" &
xscreensaver -no-splash &
if [ -n $(which battery.sh) ]; then
    battery.sh &
fi
exec spectrwm
