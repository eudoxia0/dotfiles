# Run after bootstrap.sh

# X11
sudo apt-get install -y xserver-xorg-core xserver-xorg-video-intel \
     xserver-xorg-input-evdev x11-xserver-utils xinit xdm xterm ctwm

# Sound
sudo apt-get install pavucontrol pulseaudio pulseaudio-module-zeroconf \
     alsa-utils avahi-daemon
# stochastically experiment with alsamixer and pavucontrol until it works

# Packages

sudo apt-get install -y git make emacs25 duplicity gpg sqlite3 libsqlite3-dev \
    texlive-xetex firefox-esr xcape neofetch fonts-inconsolata xfonts-terminus \
    ttf-linux-libertine clang mupdf acpi viewnior lxappearance ntp \
    xscreensaver xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra

# Java
sudo apt-get install -y openjdk-8-jdk maven

# Ruby
sudo apt-get install -y rbenv ruby-build
rbenv install 2.4.0
rbenv global 2.4.0
rbenv exec gem install jekyll

# Haskell
if ! [ -x "$(command -v stack)" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
fi

# SML
sudo apt-get install -y git mlton smlnj

# Copy files
cp gitconfig ~/.gitconfig
cp psqlrc ~/.psqlrc
cp xscreensaver ~/.xscreensaver
