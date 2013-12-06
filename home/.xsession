#!/bin/sh

./.screenlayout/home.sh &

## Ensure that the D-Bus Communication System is running properly to fix
## File management, authentication, and other essential system processes

if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    source /etc/X11/xinit/xinitrc.d/30-dbus
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK
xscreensaver -nosplash &
./rotate.sh &
pnmixer &
redshift &
xrdb .Xresources &
transmission-gtk -m &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0" 

if [ -x /usr/bin/xdg-user-dirs-update ]; then
   /usr/bin/xdg-user-dirs-update
fi

exec stumpwm
