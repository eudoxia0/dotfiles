# Personal

sudo apt-get install -y chromium-browser keepass2 feh redshift transmission-gtk \
     pcmanfm gimp inkscape gnuplot scrot calibre graphviz units pidgin \
     pidgin-otr vlc arandr cheese xdotool fonts-inconsolata xscreensaver \
     xscreensaver-gl xscreensaver-gl-extra xscreensaver-data \
     xscreensaver-data-extra ntp ntpdate xcape leafpad texlive-base \
     texlive-math-extra texlive-latex-recommended texlive-publishers \
     texlive-latex-extra  xfonts-terminus lxappearance unrar-free pavucontrol \
     youtube-dl torbrowser-launcher fonts-linuxlibertine gargoyle-free

# Programming

sudo apt-get install -y git emacs make automake autoconf libtool autogen patch \
  meld cmake gdb valgrind hunspell scala ruby clang llvm subversion python-dev \
  libxml2-dev libxslt-dev virtualenvwrapper coq proofgeneral

# Lisp

cd ~
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
  --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

# StumpWM

cd ~
git clone https://github.com/stumpwm/stumpwm.git
cd stumpwm/
autoconf
./configure
sbcl --eval '(ql:quickload (list :cl-ppcre :clx))' --quit
make
sudo make install
cd ..
rm -rf stumpwm/

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
