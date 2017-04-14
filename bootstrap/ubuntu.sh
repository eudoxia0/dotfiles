# Personal

sudo apt-get install -y chromium-browser keepass2 feh redshift transmission-gtk \
     pcmanfm gimp inkscape gnuplot scrot calibre graphviz units numlockx \
     vlc cheese xdotool fonts-inconsolata xscreensaver xscreensaver-data \
     xscreensaver-gl xscreensaver-gl-extra xscreensaver-data-extra xcape \
     leafpad unrar lxappearance youtube-dl fonts-linuxlibertine torbrowser-launcher \
     gargoyle-free qtads fonts-ancient-scripts ttf-ancient-fonts openbabel gromacs \
     libx11-dev libxft-dev libxinerama-dev \
     dmenu \
     htop \
     libimage-exiftool-perl \
     spectrwm uim

im-config -n uim

# TeX

sudo apt-get install -y texlive-base texlive-math-extra \
     texlive-latex-recommended texlive-publishers texlive-latex-extra \

# Programming

sudo apt-get install -y git subversion patch \
    make automake autoconf libtool autogen cmake \
    meld \
    gdb valgrind \
    emacs hunspell \
    llvm clang \
    coq

# Lisp

cd ~/Downloads
mkdir sbcl
cd sbcl
curl -O http://ufpr.dl.sourceforge.net/project/sbcl/sbcl/1.3.8/sbcl-1.3.8-x86-64-linux-binary.tar.bz2
curl -O http://ufpr.dl.sourceforge.net/project/sbcl/sbcl/1.3.8/sbcl-1.3.8-crhodes.asc
bunzip2 sbcl-1.3.8-x86-64-linux-binary.tar.bz2

gpg --keyserver pgp.mit.edu --recv-keys 100D63CD
gpg --verify sbcl-1.3.8-crhodes.asc
cat sbcl-1.3.8-crhodes.asc
sha256sum sbcl-1.3.8-x86-64-linux-binary.tar

tar -xvf sbcl-1.3.8-x86-64-linux-binary.tar
cd sbcl-1.3.8-x86-64-linux/
sudo bash install.sh

# Quicklisp

curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
  --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

# Ruby

sudo apt-get install -y rbenv ruby-build
rbenv install 2.1.0 -k
rbenv global 2.1.0

gem install sass
gem install jekyll

# Qt

sudo apt-get install -y qt4-qtconfig
qtconfig-qt4
