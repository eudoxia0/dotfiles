# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_AUTO_UPDATE=1

# pnpm
export PNPM_HOME="/Users/eudoxia/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# rbenv
eval "$(rbenv init - --no-rehash zsh)"

# PATH
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/sbin:/sbin"

# Aliases
alias -- 'cdd'='cd "/Users/eudoxia/Desktop/"'
alias -- 'cf'='cargo +nightly fmt'
alias -- 'ci'='cd "/Volumes/Asterion/Root/0 Inbox/"'
alias -- 'ck'='cargo check'
alias -- 'cl'='cargo clippy --all-targets -- -D warnings'
alias -- 'cr'='cd "/Volumes/Asterion/Root/"'
alias -- 'gb'='git branch'
alias -- 'gcam'='git commit -a -m'
alias -- 'gco'='git checkout'
alias -- 'gd'='git pull origin'
alias -- 'gs'='git status'
alias -- 'gu'='git push -u origin HEAD'
alias -- 'ls'='ls -1 --color'
