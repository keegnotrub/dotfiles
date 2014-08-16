# dotfiles

This is a small set of home directory config files targeted for Android and iOS development on OS X.

## Installation

After running the requirements use [Homebrew](http://brew.sh) and [RCM](https://github.com/thoughtbot/rcm):

    git clone http://github.com/keegnotrub/dotfiles ~/.dotfiles
    brew bundle ~/.dotfiles
    rcup -v -x README.md -x Brewfile

## Requirements
    # Install Xcode
    open https://itunes.apple.com/us/app/xcode/id497799835

    # Install Command Line Tools for Xcode
    xcode-select --install

    # Install Homebrew
    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
