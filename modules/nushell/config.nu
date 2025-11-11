# Hide welcome message.
$env.config.show_banner = false

# Git aliases.
alias gb   = git branch
alias gcam = git commit -a -m
alias gco  = git checkout
alias gd   = git pull origin
alias gs   = git status
alias gu   = git push -u origin HEAD

# Rust aliases.
alias cf = cargo +nightly fmt
alias ck = cargo check
alias cl = cargo clippy --all-targets -- -D warnings

# Other aliases.
alias cdt = cd ~/dotfiles
