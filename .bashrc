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
alias rvm?='rvm-prompt'

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
export LANG=en_US

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
#export HISTCONTROL=erasedups # turned off to keep all commands
export HISTSIZE=10000

# top 5 one-word commands (and total number)
function topcmd() {
    history | \
        awk "{a[\$2]++}END{print NR, \"((TOTAL))\"; for(i in a) print a[i], i}" | \
        sort -rn | \
        head -6
}

# top 5 two-word commands
function top2cmd(){
    history | \
        awk "/$1/{a[\$2 \" \" \$3]++}END{for(i in a) print a[i], i}" | \
        sort -rn | \
        head -5
}

function batt() {
    pmset -g batt | \
        awk '/InternalBattery/{print $3 $2 " " $4 }' | \
        sed -e 's/(no//' -e 's/discharging;/-/' -e 's/charging;/+/' -e 's/;/ /g'
}

# added by travis gem
[ -f /Users/mjs/.travis/travis.sh ] && source /Users/mjs/.travis/travis.sh

export NVM_DIR=$HOME/.nvm
. /usr/local/opt/nvm/nvm.sh
