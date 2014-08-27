#
# ~/.bash_profile
#

# Autostart console stuff
homeshick pull dotfiles &

# Computer-specific lisp shim
source self/.lisp-pathnames &

# Reload keymap
xmodmap .Xmodmap &

[[ -f ~/.bashrc ]] && . ~/.bashrc
