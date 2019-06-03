sh# Run after bootstrap.sh

# Sources
sudo cp sources.list /etc/apt/sources.list

sudo apt-get install -y apt-transport-https ca-certificates \
    software-properties-common

# Packages
sudo apt-get update

X11_PACKAGES="xserver-xorg-core xserver-xorg-video-intel \
xserver-xorg-input-evdev x11-xserver-utils xinit xdm xterm xdotool xcape \
ratpoison"

SOUND_PACKAGES="pavucontrol pulseaudio pulseaudio-module-zeroconf alsa-utils \
avahi-daemon"

GENERAL_PACKAGES="git make emacs25 duplicity gnupg sqlite3 libsqlite3-dev \
texlive-xetex firefox-esr xcape neofetch fonts-inconsolata xfonts-terminus \
fonts-linuxlibertine clang mupdf acpi viewnior lxappearance ntp feh gnuplot \
xscreensaver xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
transmission-gtk djview4 easytag mpv unrar ttf-ancient-fonts laptop-mode-tools \
libwebkit2gtk-4.0-37-gtk2 ocaml ocaml-native-compilers docbook-xml docbook-xsl \
docbook-xsl-ns xsltproc fop lm-sensors tlp thermald mupdf-tools ttf-mscorefonts-installer \
dmenu zotero-standalone"

JAVA_PACKAGES="openjdk-8-jdk maven"

RUBY_PACKAGES="rbenv ruby-build"

SML_PACKAGES="smlnj mlton"

sudo apt-get install -y $X11_PACKAGES $SOUND_PACKAGES $GENERAL_PACKAGES \
     $JAVA_PACKAGES $RUBY_PACKAGES $SML_PACKAGES

# Ruby
if ! [ -x "$(command -v rbenv)" ]; then
    rbenv install 2.4.0
    rbenv global 2.4.0
    rbenv exec gem install jekyll
fi

# Haskell
if ! [ -x "$(command -v stack)" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
fi

# Config files and scripts
cp gitconfig.txt ~/.gitconfig
cp psqlrc.txt ~/.psqlrc
cp xscreensaver.txt ~/.xscreensaver
cp gtkrc.txt ~/.gtkrc-2.0
cp bashrc.sh ~/.bashrc
cp stumpwmrc.lisp ~/.stumpwmrc
sudo cp dhclient.txt /etc/dhcp/dhclient.conf

mkdir -p ~/.scripts
cp battery.sh ~/.scripts/battery.sh
cp backup.sh ~/.scripts/backup.sh
cp embed_fonts.sh ~/.scripts/embed_fonts.sh
cp rotate_wallpaper.sh ~/.scripts/rotate_wallpaper.sh
chmod +x ~/.scripts/battery.sh
chmod +x ~/.scripts/backup.sh
chmod +x ~/.scripts/embed_fonts.sh

cp xcompose.txt ~/.XCompose
cp xmodmap.txt ~/.Xmodmap
cp xsession.sh ~/.xsession
cp xresources.txt ~/.Xresources

sudo cp xdm-xsetup /etc/X11/xdm/Xsetup
sudo cp xdm-xstartup /etc/X11/xdm/Xstartup
sudo chmod +x /etc/X11/xdm/Xsetup
sudo chmod +x /etc/X11/xdm/Xstartup

cp mimeapps.list ~/.local/share/applications/mimeapps.list

# Uninstall
sudo apt-get -y remove bluetooth

# Emacs
EMACS_DIR=~/.emacs.d

if [ -d "$EMACS_DIR" ]; then
  rm -rf $EMACS_DIR
fi

mkdir -p $EMACS_DIR/lang
cp -R emacs/. $EMACS_DIR

# Node
mkdir -p ~/.npm-global
npm config set prefix '~/.npm-global'
