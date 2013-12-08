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

alias api='cd ~/SRC/api && source ~/Bin/api.sh'
alias portal='cd ~/SRC/portal'
alias leadforms='cd ~/SRC/lead-forms'
alias apidbconf='~/Bin/dbconf api'
alias ffab='fab --set=DEV_SANDBOX=true,DONT_RELOAD_SANDBOX=true -H localhost setup_environment'

complete -o bashdefault -o default -o nospace -F _git g 2> /dev/null \
    || complete -o default -o nospace -F _git g
