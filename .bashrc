PATH=$HOME/Bin:/usr/local/bin:/usr/local/sbin:$PATH

[[ -f "$HOME/.githubtoken" ]] && . $HOME/.githubtoken

export CLICOLOR=1

alias df='df -h'
alias ls='ls -F'
alias rm='rm -i'
alias l=ls
alias ll='ls -l'
alias v=vi
alias m=mail
alias frm='mail -H'
alias exersub='exercism submit'
alias exerfetch='exercism fetch'
alias t='tree -C -F'

alias pdfopen='pdfopen -viewer xpdf'
alias tnefrsync="rsync -av tnef.svn.sourceforge.net::svn/tnef/* ."

export NODE_PATH=/usr/local/lib/node_modules
export MANPATH=$MANPATH:/opt/local/man
export TNEFSUBREP=https://tnef.svn.sourceforge.net/svnroot/tnef/
export EDITOR=emacsclient
export VISUAL=emacsclient

export AUTOFEATURE=true

export PYTHONSTARTUP=$HOME/.pythonrc.py

## LifeLines (genealogy software)
export LLPROGRAMS=/usr/local/share/lifelines
export LLREPORTS=$HOME/LifeLines/output
export LLARCHIVE=$HOME/LifeLines/archive
export LLDATABASES=$HOME/LifeLines

export LANG=en_US.UTF-8


###
### GIT Stuff
###
alias git=hub
function g() {
    git ${*:-status}
}
function ga() {
    git add ${*:-.}
}

alias gd='git di'
alias gdc='git dc'
alias gci='git commit -v'
alias gl='git l'
alias gla='git la'
alias gr='git r'
alias gra='git ra'

# Keeping track of my commands
shopt -s histappend
export HISTCONTROL=ignoreboth
export HISTIGNORE="ls:cd:history:bg:fg:cd:pwd"
export HISTSIZE=100000
export HISTFILESIZE=10000000
export PROMPT_COMMAND="history -a; history -n"

function batt() {
    pmset -g batt | \
        awk '/InternalBattery/{print $3 $2 " " $4 }' | \
        sed -e 's/(no//' -e 's/discharging;/-/' -e 's/charging;/+/' -e 's/;/ /g'
}

# added by travis gem
[ -f /Users/mjs/.travis/travis.sh ] && source /Users/mjs/.travis/travis.sh

export NVM_DIR=$HOME/.nvm
. /usr/local/opt/nvm/nvm.sh
