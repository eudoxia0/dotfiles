# Runs at the beginning of an X11 session.
# xsetroot -solid "#505170" # NeXTSTEP purple
# xsetroot -solid "#00807F" # Windows 95 green
xcape -e "Shift_L=parenleft;Shift_R=parenright"
~/.local/bin/wallpaper.sh &
~/.local/bin/battery.sh &
