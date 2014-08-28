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

alias sand='ssh -A -tt sandbox_dev'
alias api='sand "(cd ~/SRC/api && emacs .)"'
alias green='sand "(cd ~/SRC/greenlight && emacs .)"'
alias portal='sand "(cd ~/SRC/portal && emacs . )"'
alias klondike='sand "(cd ~/SRC/klondike && emacs .)"'
alias ltail='sand tornadotail'
alias atail='sand tornadotail api'
alias gtail='sand tornadotail greenlight'
