# Let's make Java a little less ugly (lol)

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# NPM
export PATH="$PATH:$HOME/.npm/bin:$HOME/.scripts"
