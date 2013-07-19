eval `gnome-keyring-daemon`
export GNOME_KEYRING_SOCKET &
export GNOME_KEYRING_PID &
export SSH_AUTH_SOCK="$GNOME_KEYRING_CONTROL/ssh" &
xscreensaver -nosplash &
./rotate.sh &
#transmission-gtk &

#exec awesome