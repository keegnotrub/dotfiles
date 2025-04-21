# dotfiles

This is a small set of home directory config files, targeted for Ruby on Rails development with Emacs on macOS.

## Installation

After running the requirements, use [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html)

    git clone http://github.com/keegnotrub/dotfiles
    cd dotfiles
    stow --dotfiles -t $HOME

## Requirements

    # Install Command Line Tools for Xcode
    xcode-select --install

    # Install Macports
    open https://github.com/macports/macports-base/releases
    
    # Install XQuartz
    open https://github.com/XQuartz/XQuartz/releases
    
    # Install SF Mono
    open https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg
    
    # Update ports
    sudo port -v selfupdate
    # Install Emacs
    sudo port install aspell aspell-dict-en emacs emacs-mac-app
    # Install Ruby
    sudo port install rust ruby_select ruby32 +yjit ImageMagick7 -x11
    # Install NodeJS
    sudo port install nodejs20 npm10 yarn
    # Install Redis
    sudo port install redis
    # Install MySQL
    sudo port install mysql_select mysql8-server
    # Install Postgres
    sudo port install postgresql_select-16 postgres16-server pg16-postgis3 
    # Install Nginx
    sudo port install nginx
    # Install Utils
    sudo port install git jq pwgen stow
    
    # Install pass
    git clone https://github.com/keegnotrub/pass
    cd pass
    sudo make install
    
    # Install agent
    git clone https://github.com/keegnotrub/agent
    cd agent
    sudo make install
    

## MySQL Setup

    sudo port select --set mysql mysql8
    sudo mysqld --initialize --user=_mysql
    sudo port load mysql8-server
    mysqladmin -u root -p password
     
## Postgres Setup

    sudo port select --set postgresql postgresql16
    sudo mkdir -p /opt/local/var/db/postgresql16/defaultdb
    sudo chown postgres:postgres /opt/local/var/db/postgresql16/defaultdb
    sudo -u postgres /bin/sh -c 'cd /opt/local/var/db/postgresql16 && /opt/local/lib/postgresql16/bin/initdb -D /opt/local/var/db/postgresql16/defaultdb'
    sudo -u postgres createuser --createdb --createrole keegnotrub
    sudo port load postgresql16-server

## Redis Setup

    sudo port load redis

## Nginx Setup

    sudo port load nginx

## Ruby Setup
     
    sudo port select --set ruby ruby32
