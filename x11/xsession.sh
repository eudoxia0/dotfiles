#!/bin/sh

.scripts/rotate_wallpaper &

## Ensure that the D-Bus Communication System is running properly to fix
## File management, authentication, and other essential system processes

#if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
#    source /etc/X11/xinit/xinitrc.d/30-dbus
#    eval "$(dbus-launch --sh-syntax --exit-with-session)"
#fi

# When pressed on their own (ie not modifying other keys), left and right shift
# produce a left and right parenthesis, respectively
xcape -e "Shift_L=parenleft;Shift_R=parenright"

# Autostart X stuff
xscreensaver -nosplash &
redshift &
xrdb .Xresources &
numlockx &
xsetroot -cursor_name left_ptr &
#xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &
#transmission-gtk -m &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

#if [ -x /usr/bin/xdg-user-dirs-update ]; then
#   /usr/bin/xdg-user-dirs-update
#fi

# Start dbus

#sudo dbus-launch

# Start the gnome keyring

#eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
#export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xmodmap ~/.Xmodmap
exec spectrwm
#exec dbus-launch --exit-with-session dwm
