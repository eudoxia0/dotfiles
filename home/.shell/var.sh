# Let's make Java a little less ugly (lol)

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# less
export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
export LESS=' -R '

# NPM
export PATH="$PATH:$HOME/.npm/bin:$HOME/.scripts:$HOME/.cabal/bin"

# Ruby
export PATH="$PATH:$HOME/.gem/ruby/2.1.0/bin"

# General
PS1="\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$\[\033[01m\][ \[\033[01;34m\]\u@\h \[\033[00m\]\[\033[01m\]] \[\033[01;32m\]\w\[\033[00m\]\n\[\033[01;34m\]$\[\033[00m\]> "

export EDITOR="emacs"
