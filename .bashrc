PATH=$HOME/Bin:$PATH

export BREW_PREFIX=`brew --prefix`

export EDITOR=emacsclient
export VISUAL=emacsclient

export PYTHONSTARTUP=$HOME/.pythonrc.py

export LANG=en_US.UTF-8

## LifeLines (genealogy software)
export LLPROGRAMS=$BREW_PREFIX/share/lifelines
export LLREPORTS=$HOME/LifeLines/output
export LLARCHIVE=$HOME/LifeLines/archive
export LLDATABASES=$HOME/LifeLines

###
### GIT Stuff
###
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
alias gp='git push'

# Keeping track of my commands
shopt -s histappend
export HISTCONTROL=ignoreboth
export HISTIGNORE="ls:cd:history:bg:fg:cd:pwd"
export HISTSIZE=100000
export HISTFILESIZE=10000000
export PROMPT_COMMAND="history -a; history -n"

alias frm='mail -H'

function batt() {
    pmset -g batt | \
        awk '/InternalBattery/{print $3 $2 " " $4 }' | \
        sed -e 's/(no//' -e 's/discharging;/-/' -e 's/charging;/+/' -e 's/;/ /g'
}

function _log() {
    echo `date +%FT%T%z` ' -- ' $*
}
