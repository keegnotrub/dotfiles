# dotfiles

This is a small set of home directory config files targeted for OS X 10.8.

## Installation

    git clone https://github.com/elbongurk/dotfiles
    cd dotfiles
    ln -s profile ~/.profile
    ln -s gemrc ~/.gemrc

## Requirements

    # Install Homebrew
    ruby <(curl -fsS https://raw.github.com/mxcl/homebrew/go)
    brew update
    brew install postgres rbenv ruby-build openssl git heroku-toolbelt

    # Install Ruby
    CONFIGURE_OPTS="--with-openssl-dir=`brew --prefix openssl`" rbenv install 2.0.0-p195
    rbenv global 2.0.0-p195

    # Install Gems
    gem update --system
    gem install bundler rails
