# Let's make Java a little less ugly (lol)

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

# NPM
export PATH="$PATH:$HOME/.npm/bin:$HOME/.scripts"

# Ruby
export PATH="$PATH:$HOME/.gem/ruby/2.1.0/bin"

#Lisp
CIM_HOME=/home/eudoxia/.cim; [ -s '/home/eudoxia/.cim/init.sh' ] && . '/home/eudoxia/.cim/init.sh'

# General
PS1="\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$\[\033[01m\][ \[\033[01;34m\]\u@\h \[\033[00m\]\[\033[01m\]] \[\033[01;32m\]\w\[\033[00m\]\n\[\033[01;34m\]$\[\033[00m\]> "
