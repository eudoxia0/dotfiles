# Apps

sudo apt-get install -y chromium keepass2 git feh redshift transmission-gtk \
  emacs pcmanfm gimp inkscape keepass2 gnuplot scrot calibre numlockx graphviz \
  virtualbox units newsbeuter pidgin pidgin-otr vlc arandr cheese xdotool \
  flashplugin-nonfree zathura zathura-djvu zathura-ps fonts-inconsolata \
  xscreensaver xscreensaver-gl xscreensaver-gl-extra xscreensaver-data \
  xscreensaver-data-extra ntp ntpdate xcape curl xfonts-terminus shotwell

# Dev stuff

sudo apt-get install -y make automake autoconf libtool autogen patch meld \
  cmake gdb valgrind hunspell scala ruby clang llvm subversion python-dev \
  libxml2-dev libxslt-dev virtualenvwrapper coq proofgeneral swi-prolog

# Build SBCL

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

# Build Electrum

sudo apt-get install -y python-qt4 python-pip
sudo pip install https://download.electrum.org/Electrum-2.0.3.tar.gz

# Build StumpWM

git clone https://github.com/stumpwm/stumpwm.git
cd stumpwm/
autoconf
./configure
sbcl --eval '(ql:quickload (list :cl-ppcre :clx))' --quit
make
sudo make install

# Ruby

git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
git clone https://github.com/sstephenson/rbenv-gem-rehash.git ~/.rbenv/plugins/rbenv-gem-rehash

sudo apt-get install -y build-essential libssl-dev libcurl4-openssl-dev \
  libreadline-dev

# Python

sudo apt-get install python-pip
sudo pip install ansible

# Vagrant

curl -o vagrant.deb https://dl.bintray.com/mitchellh/vagrant/vagrant_1.7.2_x86_64.deb
sudo dpkg -i vagrant.deb
rm vagrant.deb

# Haskell

wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/debian/jessie stable main'| sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get update
sudo apt-get install -y stack

# LaTeX

sudo apt-get install -y texlive-base texlive-math-extra texlive-latex-recommended \
    texlive-publishers texlive-latex-extra
