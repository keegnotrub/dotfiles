# dotfiles

This is a small set of home directory config files targeted for Ruby and iOS development on OS X.

## Installation

After running the requirements use [RCM](https://github.com/thoughtbot/rcm):

    git clone http://github.com/keegnotrub/dotfiles ~/.dotfiles
    rcup -v -x README.md

## Requirements

    # Install Homebrew
    ruby <(curl -fsS https://raw.github.com/mxcl/homebrew/go)
    brew update
    brew tap thoughtbot/formulae
    brew install postgres rbenv ruby-build heroku-toolbelt hub git bash-completion liftoff rcm

    # Setup Postgres
    initdb /usr/local/var/postgres -E utf8
    ln -sfv /usr/local/opt/postgresql/*.plist ~/Library/LaunchAgents
    launchctl load ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist

    # Setup Ruby
    rbenv install 2.1.2
    rbenv global 2.1.2

    # Install Gems
    gem update --system
    gem install bundler rails
    rbenv rehash
