# ~/.bash_profile

# Force GTK and QT to use .XCompose
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"

[[ -f ~/.bashrc ]] && . ~/.bashrc

# OPAM configuration
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
