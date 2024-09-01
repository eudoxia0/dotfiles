# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_AUTO_UPDATE=1

# Variables
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/sbin:/sbin"

# Aliases
alias -- 'cf'='cargo fmt'
alias -- 'ck'='cargo check'
alias -- 'gb'='git branch'
alias -- 'gcam'='git commit -a -m'
alias -- 'gco'='git checkout'
alias -- 'gd'='git pull origin'
alias -- 'gs'='git status'
alias -- 'gu'='git push -u origin HEAD'
alias -- 'ls'='ls -1 --color'
