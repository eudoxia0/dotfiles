# Config files and scripts
mkdir -p ~/.config/git/
cp gitconfig.txt ~/.config/git/config

cp xscreensaver.txt ~/.xscreensaver
cp gtkrc.txt ~/.gtkrc-2.0
cp bashrc.sh ~/.bashrc

mkdir -p ~/.config/spectrwm/
cp spectrwm.conf ~/.config/spectrwm/spectrwm.conf

mkdir -p ~/.emacs.d/
cp init.el ~/.emacs.d/init.el

mkdir -p ~/.scripts
cp battery.sh ~/.scripts/battery.sh
cp backup.sh ~/.scripts/backup.sh
cp embed_fonts.sh ~/.scripts/embed_fonts.sh
cp rotate_wallpaper.sh ~/.scripts/rotate_wallpaper.sh
chmod +x ~/.scripts/battery.sh
chmod +x ~/.scripts/backup.sh
chmod +x ~/.scripts/embed_fonts.sh
chmod +x ~/.scripts/rotate_wallpaper.sh

mkdir -p ~/.config/x11/
cp xcompose.txt ~/.XCompose
cp xmodmap.txt ~/.config/x11/xmodmap
cp xsession.sh ~/.xsession
cp xresources.txt ~/.config/x11/xresources
