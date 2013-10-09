if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi
if [ -f /usr/local/Library/Contributions/brew_bash_completions.sh ]; then
    . /usr/local/Library/Contributions/brew_bash_completions.sh
fi

. $HOME/.bashrc

CDPATH=.:~:~/SRC:~/Documents:~/Movies

