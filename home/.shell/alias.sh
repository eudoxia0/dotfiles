alias dwenv='deactivate;workon dwenv'
alias ttenv='deactivate;workon ttenv'
alias dwpenv='deactivate;workon dwp_env'

alias ls="ls --group-directories-first --color=auto -h "
alias reboot="sudo reboot"
alias shd="sudo shutdown now -h"
alias cls="printf \"\\033c\""
alias lock='xscreensaver-command -lock'
alias homeshick="$HOME/.homesick/repos/homeshick/home/.homeshick"
alias psql='psql -h localhost'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
    alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
    alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
    alias grep='grep --color=tty -d skip'
    alias egrep='egrep --color=auto'
fi

export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
export LESS=' -R '
