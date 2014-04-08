# Let's make Java a little less ugly (lol)

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

# NPM
export PATH="$PATH:$HOME/.npm/bin:$HOME/.scripts"

# Ruby
export PATH="$PATH:$HOME/.gem/ruby/2.1.0/bin"

CIM_HOME=/home/eudoxia/.cim; [ -s '/home/eudoxia/.cim/init.sh' ] && . '/home/eudoxia/.cim/init.sh'
