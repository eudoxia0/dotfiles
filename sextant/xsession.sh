xmodmap ~/.Xmodmap
xrdb ~/.Xresources
xcape -e "Shift_L=parenleft;Shift_R=parenright"
xscreensaver -no-splash &
.scripts/battery.sh &
feh --bg-scale ~/.walls/view-of-dresden-by-moonlight-johan-christian-dahl.jpg

exec stumpwm
