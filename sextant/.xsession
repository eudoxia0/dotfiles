rotate_wallpaper.sh &
#xsetroot -solid '#505170'
xmodmap ~/.config/x11/xmodmap &
xrdb ~/.config/x11/xresources &
xcape -e "Shift_L=parenleft;Shift_R=parenright" &
xscreensaver -no-splash &
#.scripts/battery.sh &
#redshift &

# NOTE: this is how you shut down the backlight on the ASUS laptop
# sudo bash -c 'echo 0 > /sys/devices/platform/asus-nb-wmi/leds/asus\:\:kbd_backlight/brightness'

exec spectrwm
