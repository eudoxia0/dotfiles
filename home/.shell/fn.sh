# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# lsp - run a Common Lisp file as a script
# usage: lsp <file>
lsp () {
  sbcl --noinform --load $1 --quit
}

# dial - ssh into a host defined in .shell/hosts.txt
# usage: dial <host>
# depends on: .shell/hosts.txt
dial () {
  ssh `grep $1 ~/.shell/hosts.txt | tr -s ' ' | cut -d ' ' -f 2`
}

# geo - get the location of a host
# usage: geo <host>
# output: <country>,<state>,<lat>,<lon>
geo () {
  local DATA=`curl http://www.freegeoip.net/csv/$1 -o /tmp/.geoip -s`
  local OUT="cat /tmp/.geoip | tr -d '\"' | cut -d ',' -f "
  local COUNTRY=`eval ${OUT} 3`
  local STATE=`eval ${OUT} 4`
  local LAT=`eval ${OUT} 8`
  local LON=`eval ${OUT} 9`
  rm /tmp/.geoip
  echo $COUNTRY,$STATE,$LAT,$LON
}

# world_time - print the time in different timezones
# usage: world_time
# slightly modified from: https://github.com/isislovecruft/scripts
world_time () {
    local datetime="date +%H:%M"
    echo -e 'UTC\t\t' `TZ="Europe/Reijkjavik" $datetime`
    echo -e 'Berlin\t\t' `TZ="Europe/Berlin" $datetime`
    echo -e 'Adelaide\t' `TZ="Australia/Adelaide" $datetime`
    echo -e 'Tokyo\t\t' `TZ="Asia/Tokyo" $datetime`
}

# encrypt, decrypt - encrypt and decrypt a folder with a password
# usage: [encrypt|decrypt] <file>
# from: http://superuser.com/questions/162624/how-to-password-protect-gzip-files-on-the-command-line
encrypt () {
    tar cz $1 | openssl enc -aes-256-cbc -e > $1.tar.gz.enc
}
decrypt () {
    openssl aes-256-cbc -d -in $1.tar.gz.enc -out $1.decrypted.tar.gz
}

# lisp_up - update quicklisp local projects
# usage: lisp_up
lisp_up () {
    find ~/quicklisp/local-projects \
        -mindepth 1 -maxdepth 1 -type d -exec git pull origin master
}
