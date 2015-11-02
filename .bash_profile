source $HOME/.profile

if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
    __git_complete g __git_main
fi
if [ -f /usr/local/Library/Contributions/brew_bash_completions.sh ]; then
    . /usr/local/Library/Contributions/brew_bash_completions.sh
fi

. $HOME/.bashrc

CDPATH=.:~:~/SRC:~/Documents:~/Movies

# RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -r "$HOME/.rvm/scripts/completion" ]] && . "$HOME/.rvm/scripts/completion"
