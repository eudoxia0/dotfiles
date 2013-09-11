## Ensure that the D-Bus Communication System is running properly to fix
## File management, authentication, and other essential system processes

if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi


eval `gnome-keyring-daemon`
export GNOME_KEYRING_SOCKET &
export GNOME_KEYRING_PID &
export SSH_AUTH_SOCK="$GNOME_KEYRING_CONTROL/ssh" &
xscreensaver -nosplash &
./rotate.sh &
nm-applet &
transmission-gtk &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0" 

if [ -x /usr/bin/xdg-user-dirs-update ]; then
   /usr/bin/xdg-user-dirs-update
fi

exec awesome
