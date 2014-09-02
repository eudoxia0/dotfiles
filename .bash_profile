#
# ~/.bash_profile
#

# Computer-specific lisp shim
source self/.lisp-pathnames &

# Reload keymap
xmodmap .Xmodmap &

[[ -f ~/.bashrc ]] && . ~/.bashrc
