.scripts/rotate_wallpaper.sh &
xmodmap ~/.Xmodmap &
xrdb ~/.Xresources &
xcape -e "Shift_L=parenleft;Shift_R=parenright" &
xscreensaver -no-splash &
.scripts/battery.sh &

# NOTE: this is how you shut down the backlight on the ASUS laptop
# sudo bash -c 'echo 0 > /sys/devices/platform/asus-nb-wmi/leds/asus\:\:kbd_backlight/brightness'

exec stumpwm
