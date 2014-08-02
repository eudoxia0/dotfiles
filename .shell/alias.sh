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

alias dotfiles="cd ~/.homesick/repos/dotfiles/home/"
