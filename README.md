# dotfiles

This is a small set of home directory config files targeted for GitHub's codespaces.

## Installation

Adjust your [codespaces settings](https://github.com/settings/codespaces) to `Automatically install dotfiles` from [keegnotrub/dotfiles](https://github.com/keegnotrub/dotfiles)

## Requirements

    # Install Command Line Tools for Xcode
    xcode-select --install

    # Install Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    
    # Install GitHub CLI
    brew install gh
    
    # Authenticate GitHub CLI
    gh auth login
