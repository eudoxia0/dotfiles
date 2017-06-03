# Personal

sudo apt-get install -y chromium-browser keepass2 feh redshift transmission-gtk \
     pcmanfm gimp inkscape gnuplot scrot calibre graphviz units numlockx \
     vlc cheese xdotool fonts-inconsolata xscreensaver xscreensaver-data \
     xscreensaver-gl xscreensaver-gl-extra xscreensaver-data-extra xcape \
     leafpad unrar lxappearance youtube-dl fonts-linuxlibertine torbrowser-launcher \
     gargoyle-free qtads fonts-ancient-scripts ttf-ancient-fonts openbabel gromacs \
     libx11-dev libxft-dev libxinerama-dev \
     htop \
     libimage-exiftool-perl \
     uim

im-config -n uim

# TeX

sudo apt-get install -y texlive-base texlive-math-extra \
     texlive-latex-recommended texlive-publishers texlive-latex-extra \
     texinfo

# Programming

sudo apt-get install -y git subversion patch \
    make automake autoconf libtool autogen cmake \
    meld \
    gdb valgrind \
    emacs hunspell \
    llvm clang \
    coq \
    sbcl

# Quicklisp

curl -o /tmp/ql.lisp https://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval "(quicklisp-quickstart:install :path \"$HOME/.quicklisp\")" \
  --quit

# StumpWM

sbcl --noinform --eval '(ql:quickload (list :clx :cl-ppcre :alexandria))' --quit
cd ~/Downloads
git clone https://github.com/stumpwm/stumpwm.git
cd stumpwm
./autogen.sh
./configure
make
sudo make install

# Ruby

sudo apt-get install -y rbenv ruby-build
rbenv install 2.4.0 -k
rbenv global 2.4.0

gem install sass
gem install jekyll

# Qt

sudo apt-get install -y qt4-qtconfig
qtconfig-qt4
