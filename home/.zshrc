# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_AUTO_UPDATE=1

# PATH
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/sbin:/sbin"
export PATH="$PATH:/opt/homebrew/lib/ruby/gems/3.3.0/bin"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"

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
