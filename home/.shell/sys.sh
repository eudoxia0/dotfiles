case "$OSTYPE" in
  linux*)   export LINUX=true ;;
  bsd*)     export BSD=true ;;
esac

if [ $LINUX ]; then
    if [ "$(which apt-get)" ]; then
        alias ins='sudo apt-get install -y'
        alias un='sudo apt-get purge -y'
        alias up='sudo apt-get upgrade -y'
        alias sup='sudo apt-get dist-upgrade -y'
        alias syn='sudo apt-get update'
        alias clean='sudo apt-get autoremove -y'
    elif [ "$(which pacman)" ]; then
        alias ins='sudo pacman -S'
        alias un='sudo pacman -Rns'
        alias up='sudo pacman -Su'
        alias sup='sudo pacman -Su'
        alias syn='sudo pacman -y'
    fi
fi

if [ $BSD ]; then
    # Assume its FreeBSD
    function ins() { cd /usr/ports/$1; sudo make clean install $2; }
fi
