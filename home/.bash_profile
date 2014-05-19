#
# ~/.bash_profile
#

# Autostart console stuff
homeshick pull dotfiles &

# Computer-specific lisp shim
source self/.lisp-pathnames &

[[ -f ~/.bashrc ]] && . ~/.bashrc
