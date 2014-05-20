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

# dial - ssh into a host defined in .shell/hosts.txt
# usage: dial <host>
# depends on: .shell/hosts.txt
dial () {
  ssh `grep $1 ~/self/hosts.txt | tr -s ' ' | cut -d ' ' -f 2-10`
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
    echo -e 'Houston\t\t' `TZ="US/Central" $datetime`
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

# colored man pages
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}


cl_compile() {
    sbcl --noinform --eval "(compile-file \"$1\")" --quit
}

cl_compile_all() {
    find . -name '*.lisp*' -exec cl_compile {} \;
}

sbcl_gencore() {
    sbcl --eval '(sb-ext:save-lisp-and-die "sbcl.core")' --quit
}
