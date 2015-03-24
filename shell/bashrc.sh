# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# Aliases
alias ls="ls --group-directories-first --color=auto -h "
alias reboot="sudo reboot"
alias shd="sudo shutdown now -h"
alias cls="printf \"\\033c\""
alias lock='xscreensaver-command -lock'
alias psql='psql -h localhost'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
    alias grep='grep --color=tty -d skip'
    alias egrep='egrep --color=auto'
fi

# Functions

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

# last_boot - the time the computer was started
last_boot () {
    last | less | head -n 1 | cut -d ' ' -f 11
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

# Variables

# Let's make Java a little less ugly (lol)

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# less
export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
export LESS=' -R '

# NPM
export PATH="$PATH:$HOME/.npm/bin:$HOME/.scripts:$HOME/.cabal/bin"

# Ruby
export PATH="$PATH:$HOME/.rbenv/versions/2.1.0/bin"
export PATH="$PATH:$HOME/.rbenv/versions/2.2.0-dev/bin"

# General
PS1="\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$\[\033[01m\][ \[\033[01;34m\]\u@\h \[\033[00m\]\[\033[01m\]] \[\033[01;32m\]\w\[\033[00m\]\n\[\033[01;34m\]$\[\033[00m\]> "

export EDITOR="emacs"

# Android
export ANDROID_SDK_ROOT="/home/eudoxia/android/adt-bundle-linux-x86_64-20140702/"
export ANDROID_NDK_ROOT="/home/eudoxia/android/android-ndk-r10b/"

# Autocomplete

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# rbenv
eval "$(rbenv init -)"
