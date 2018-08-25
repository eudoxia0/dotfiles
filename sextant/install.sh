# Run after bootstrap.sh

sudo apt-get update

# X11
sudo apt-get install -y xserver-xorg-core xserver-xorg-video-intel \
    xserver-xorg-input-evdev x11-xserver-utils xinit xdm xterm xdotool xcape \
    stumpwm

# Sound
sudo apt-get install -y pavucontrol pulseaudio pulseaudio-module-zeroconf \
    alsa-utils avahi-daemon
# stochastically experiment with alsamixer and pavucontrol until it works

# Packages
sudo apt-get install -y git make emacs25 duplicity gpg sqlite3 libsqlite3-dev \
    texlive-xetex firefox-esr xcape neofetch fonts-inconsolata xfonts-terminus \
    ttf-linux-libertine clang mupdf acpi viewnior lxappearance ntp feh gnuplot \
    xscreensaver xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
    git transmission-gtk djview4 kid3-qt mpv unrar ttf-ancient-fonts

# Java
sudo apt-get install -y openjdk-8-jdk maven

# Ruby
if ! [ -x "$(command -v rbenv)" ]; then
    sudo apt-get install -y rbenv ruby-build
    rbenv install 2.4.0
    rbenv global 2.4.0
    rbenv exec gem install jekyll
fi

# Haskell
if ! [ -x "$(command -v stack)" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
fi

# SML
sudo apt-get install -y mlton smlnj

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
chmod +x ~/.scripts/battery.sh
chmod +x ~/.scripts/backup.sh
chmod +x ~/.scripts/embed_fonts.sh

cp xcompose.txt ~/.XCompose
cp modmap.txt ~/.Xmodmap
cp xsession.sh ~/.xsession
cp xresources.txt ~/.Xresources

sudo cp xdm-xsetup /etc/X11/xdm/Xsetup
sudo cp xdm-xstartup /etc/X11/xdm/Xstartup
sudo chmod +x /etc/X11/xdm/Xsetup
sudo chmod +x /etc/X11/xdm/Xstartup

# Wallpapers
mkdir -p ~/.walls
cp -R wallpapers/. ~/.walls

# Uninstall
sudo apt-get -y remove bluetooth

# Emacs
EMACS_DIR=~/.emacs.d

if [ -d "$EMACS_DIR" ]; then
  rm -rf $EMACS_DIR
fi

mkdir -p $EMACS_DIR/lang
cp -R emacs/. $EMACS_DIR
