#!/bin/sh -ex

echo "BASICS"

SRC=`dirname $0`

ln -vs $SRC/.???* .
ln -vs $SRC/Bin .

rm .git

echo "HOMEBREW"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval $(/opt/homebrew/bin/brew shellenv)
brew bundle install ~/.Brewfile

echo "LAUNCHBAR SCRIPTS"
cp Files/*.applescript '~/Library/Application Support/LaunchBar/Actions'

echo "FILE/PROCESS LIMITS"
sudo cp Files/limit.maxfiles.plist /Library/LaunchDaemons/
sudo launchctl load -w /Library/LaunchDaemons/limit.maxfiles.plist
sudo cp Files/limit.maxprocs.plist /Library/LaunchDaemons/
sudo launchctl load -w /Library/LaunchDaemons/limit.maxprocs.plist

echo "POSTFIX - see README"

echo "FETCHMAIL"
chmod 0700 .fetchmailrc
cp Files/mark.fetchmail.plist ~/Library/LaunchAgents/
echo "See README for next steps for fetchmail
