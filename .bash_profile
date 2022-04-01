if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi

if [ -f $BREW_PREFIX/etc/bash_completion ]; then
    . $BREW_PREFIX/etc/bash_completion
    __git_complete g __git_main
fi

eval `gh completion -s bash`

reset=$'\e[0m'
cyan=$'\e[0;36m'
yellow=$'\e[0;33m'
red=$'\e[0;31m'
purple=$'\e[0;35m'
green=$'\e[0;32m'
blue=$'\e[0;34m'
grey=$'\e[1;30m'

function _pscolor() {
    if [ $? -ne 0 ]; then
        echo -e $red
    else
        echo -e $cyan
    fi
}
export PS1='\[$(_pscolor)\]\A \h:\W\[${reset}\]> '

eval "$(rbenv init -)"

export NVM_DIR=$HOME/.nvm
if [ -f $BREW_PREFIX/opt/nvm/nvm.sh ]; then
    source $BREW_PREFIX/opt/nvm/nvm.sh
fi

export BASH_SILENCE_DEPRECATION_WARNING=1

mail -H
