# Run after bootstrap.sh

set -euxo pipefail

# Sources
sudo cp sources.list /etc/apt/sources.list

sudo apt-get install -y apt-transport-https ca-certificates \
    software-properties-common

# Packages
sudo apt-get update

X11_PACKAGES="xserver-xorg-core xserver-xorg-video-intel \
xserver-xorg-input-evdev x11-xserver-utils xinit xdm xterm xdotool xcape \
xscreensaver xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
stumpwm"

SOUND_PACKAGES="pavucontrol pulseaudio pulseaudio-module-zeroconf alsa-utils \
avahi-daemon"

DEV_PACKAGES="git make emacs25 clang gnuplot ocaml ocaml-native-compilers \
smlnj mlton jekyll"

GENERAL_PACKAGES="duplicity gnupg sqlite3 libsqlite3-dev \
texlive-xetex firefox-esr neofetch fonts-inconsolata xfonts-terminus \
fonts-linuxlibertine mupdf acpi viewnior lxappearance ntp feh \
transmission-gtk djview4 easytag mpv unrar-free ttf-ancient-fonts \
libwebkit2gtk-4.0-37-gtk2 docbook-xml docbook-xsl \
docbook-xsl-ns xsltproc fop lm-sensors tlp thermald mupdf-tools \
zotero-standalone clementine pcmanfm screenfetch keepassx calibre xarchiver htop ansible inxi arc-theme redshift"

sudo apt-get install -y $X11_PACKAGES $SOUND_PACKAGES $DEV_PACKAGES \
     $GENERAL_PACKAGES

# Config files and scripts
mkdir -p ~/.config/git/
cp gitconfig.txt ~/.config/git/config
cp psqlrc.txt ~/.psqlrc
cp xscreensaver.txt ~/.xscreensaver
cp gtkrc.txt ~/.gtkrc-2.0
cp bashrc.sh ~/.bashrc
mkdir -p ~/.config/stumpwm/
cp stumpwmrc.lisp ~/.config/stumpwm/config
sudo cp dhclient.txt /etc/dhcp/dhclient.conf

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
cp xresources.txt ~/.Xresources

sudo cp xdm-xsetup /etc/X11/xdm/Xsetup
sudo cp xdm-xstartup /etc/X11/xdm/Xstartup
sudo chmod +x /etc/X11/xdm/Xsetup
sudo chmod +x /etc/X11/xdm/Xstartup

cp mimeapps.list ~/.local/share/applications/mimeapps.list

# Remove the directories you start out with in your home directory
function rem() {
  if [ -d $1 ]
  then
    rmdir ~/$1
  fi
}

rem Documents
rem Pictures
rem Templates
rem Desktop
rem Downloads
rem Music
rem Public
rem Videos

# Remove unnecessary packages
sudo apt-get purge -y aptitude debian-faq doc-debian ispell vim-tiny bluetooth

# Remove orphans
# find orphans with `sudo deborphan --guess-all`
sudo apt-get purge -y vim-common libswitch-perl libxapian30 libclass-isa-perl

# Emacs
EMACS_DIR=~/.emacs.d

if [ -d "$EMACS_DIR" ]; then
  rm -rf $EMACS_DIR
fi

mkdir -p $EMACS_DIR/lang
cp -R emacs/. $EMACS_DIR
