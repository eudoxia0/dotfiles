# Personal

sudo apt-get install -y chromium-browser keepass2 feh redshift transmission-gtk \
     pcmanfm gimp inkscape gnuplot scrot calibre graphviz units \
     vlc cheese xdotool fonts-inconsolata xscreensaver xscreensaver-data \
     xscreensaver-gl xscreensaver-gl-extra xscreensaver-data-extra xcape \
     leafpad unrar lxappearance youtube-dl fonts-linuxlibertine torbrowser-launcher \
     gargoyle-free qtads fonts-ancient-scripts ttf-ancient-fonts openbabel gromacs

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

cd ~
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

# Docker

sudo apt-get install -y apt-transport-https ca-certificates
sudo apt-key adv \
  --keyserver hkp://p80.pool.sks-keyservers.net:80 \
  --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
# Add docker to sources.list
sudo gedit /etc/apt/sources.list.d/docker.list
sudo apt-get update
sudo apt-get install -y docker-engine
sudo systemctl enable docker
sudo usermod -aG docker eudoxia

sudo apt-get install -y python-pip

# Qt

sudo apt-get install -y qt4-qtconfig
qtconfig-qt4
