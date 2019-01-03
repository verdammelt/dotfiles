#!/bin/sh -ex

echo "BASICS"

SRC=`dirname $0`

ln -vs $SRC/.???* .
ln -vs $SRC/Bin .

rm .git
chmod 0700 .fetchmailrc

echo "HOMEBREW"

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
cat $SRC/Files/brew-cask-list | xargs -n 1 brew cask install
cat $SRC/Files/brew-list | xargs -n 1 brew install

echo "RVM"
curl -sSL https://get.rvm.io | bash -s stable

echo "LAUNCHBAR SCRIPTS"
cp Files/*.applescript ~/Library/Application Support/LaunchBar/Actions
