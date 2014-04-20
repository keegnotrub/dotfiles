# dotfiles

This is a small set of home directory config files targeted for Ruby development on OS X.

## Installation

    git clone https://github.com/keegnotrub/dotfiles
    cd dotfiles
    ln -s `pwd`/profile ~/.profile
    ln -s `pwd`/gemrc ~/.gemrc
    ln -s `pwd`/gitconfig ~/.gitconfig

## Requirements

    # Install Homebrew
    ruby <(curl -fsS https://raw.github.com/mxcl/homebrew/go)
    brew update
    brew install postgres rbenv ruby-build heroku-toolbelt hub git bash-completion

    # Setup Postgres
    initdb /usr/local/var/postgres -E utf8
    ln -sfv /usr/local/opt/postgresql/*.plist ~/Library/LaunchAgents
    launchctl load ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist

    # Setup Ruby
    rbenv install 2.1.1
    rbenv global 2.1.1

    # Install Gems
    gem update --system
    gem install bundler rails
    rbenv rehash
