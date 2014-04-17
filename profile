# Homebrew
export PATH="/usr/local/bin:/usr/local/sbin:${PATH}"

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# rbenv
export RBENV_ROOT=/usr/local/var/rbenv
export PATH="${RBENV_ROOT}/shims:${PATH}"

# Emacs
export EDITOR=emacs

# Aliases
alias ls='ls -FG'
alias git=hub
