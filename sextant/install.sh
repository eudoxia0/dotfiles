# Config files and scripts
mkdir -p ~/.config/git/
cp gitconfig.txt ~/.config/git/config
cp psqlrc.txt ~/.psqlrc
cp xscreensaver.txt ~/.xscreensaver
cp gtkrc.txt ~/.gtkrc-2.0
cp bashrc.sh ~/.bashrc
mkdir -p ~/.config/stumpwm/
cp stumpwmrc.lisp ~/.config/stumpwm/config
mkdir -p ~/.config/spectrwm/
cp spectrwm.conf ~/.config/spectrwm/spectrwm.conf
sudo cp dhclient.txt /etc/dhcp/dhclient.conf
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

sudo cp xdm-xsetup /etc/X11/xdm/Xsetup
sudo cp xdm-xstartup /etc/X11/xdm/Xstartup
sudo chmod +x /etc/X11/xdm/Xsetup
sudo chmod +x /etc/X11/xdm/Xstartup

cp mimeapps.list ~/.local/share/applications/mimeapps.list
