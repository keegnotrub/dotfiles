# dotfiles

This is a small set of home directory config files targeted for Ruby development on OS X.

## Installation

    git clone https://github.com/elbongurk/dotfiles
    cd dotfiles
    ln -s `pwd`/profile ~/.profile
    ln -s `pwd`/gemrc ~/.gemrc

## Requirements

    # Install Homebrew
    ruby <(curl -fsS https://raw.github.com/mxcl/homebrew/go)
    brew update
    brew install postgres heroku-toolbelt

    # Setup Postgres
    initdb /usr/local/var/postgres -E utf8
    ln -sfv /usr/local/opt/postgresql/*.plist ~/Library/LaunchAgents
    launchctl load ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist

    # Install Gems
    sudo gem update --system
    sudo gem install bundler rails
