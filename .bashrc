[[ -s "/Users/mark/.rvm/scripts/rvm" ]] && source "/Users/mark/.rvm/scripts/rvm"
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion
[[ -f "$HOME/.githubtoken" ]] && . $HOME/.githubtoken

export CLICOLOR=1

alias df='df -h'
alias ls='ls -F'
alias rm='rm -i'
alias l=ls
alias ll='ls -l'
alias v=vi
alias m=mail

alias pdfopen='pdfopen -viewer xpdf'
alias tnefrsync="rsync -av tnef.svn.sourceforge.net::svn/tnef/* ."

PATH=$HOME/Bin:$HOME/.rvm/bin:/usr/local/bin:$PATH
NODE_PATH=/usr/local/lib/node_modules
MANPATH=$MANPATH:/opt/local/man
TNEFSUBREP=https://tnef.svn.sourceforge.net/svnroot/tnef/
EDITOR=vim
VISUAL=vim

export AUTOFEATURE=true

## LifeLines (genealogy software)
alias llines='(cd $HOME/LifeLines ; llines simpson)'
export LLPROGRAMS=/usr/local/share/lifelines
export LLREPORTS=$HOME/LifeLines/output
export LLARCHIVE=$HOME/LifeLines/archive
export LLDATABASES=$HOME/LifeLines

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
alias gci='git commit -v'


export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWUNTRACKEDFILES=1

reset='\[\e[0m\]'
cyan='\[\e[0;36m\]'
yellow='\[\e[0;33m\]'
red='\[\e[0;31m\]'
purple='\[\e[0;35m\]'
green='\[\e[0;32m\]'
blue='\[\e[0;34m\]'
grey='\[\e[1;30m\]'

#export PS1='\[\033[G\]\h:\W'$purple'$(__git_ps1 "(%s)")'$reset'> '
export PS1='\[\e[0;34m\e[40m\]\A \h:\W'$reset$purple'$(__git_ps1 "(%s)")'$reset'> '

function myip(){ 
    ip=`curl -s automation.whatismyip.com/n09230945.asp` 
    echo $ip | pbcopy
    echo $ip
}

# Keeping track of my commands
#export HISTCONTROL=erasedups # turned off to keep all commands
export HISTSIZE=5000

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

