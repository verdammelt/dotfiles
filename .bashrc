[[ -s "/Users/mark/.rvm/scripts/rvm" ]] && source "/Users/mark/.rvm/scripts/rvm"
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion

export CLICOLOR=1
alias ls="ls -F"
alias rm="rm -i"
alias fat="(cd ~/SRC/thefattrack && rails s)"
alias git=hub
alias pdfopen='pdfopen -viewer xpdf'


PATH=$HOME/Bin:$HOME/.rvm/bin:/usr/local/bin:$PATH

MANPATH=$MANPATH:/opt/local/man
TNEFSUBREP=https://tnef.svn.sourceforge.net/svnroot/tnef/
EDITOR=vim
VISUAL=vim
export LLPROGRAMS=/opt/local/share/lifelines
export LLREPORTS=$HOME/LifeLines/output
export LLARCHIVE=$HOME/LifeLines/archive
export LLDATABASES=$HOME/LifeLines

export AUTOFEATURE=true

alias tnefrsync="rsync -av tnef.svn.sourceforge.net::svn/tnef/* ."

alias llines='(cd $HOME/LifeLines ; llines simpson)'

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILED=1

reset='\[\e[0m\]'
cyan='\[\e[0;36m\]'
yellow='\[\e[0;33m\]'
red='\[\e[0;31m\]'
purple='\[\e[0;35m\]'
green='\[\e[0;32m\]'
blue='\[\e[0;34m\]'
grey='\[\e[1;30m\]'

export PS1='\h:\W'$purple'$(__git_ps1 "(%s)")'$reset'> '

function myip(){ 
    ip=`curl -s automation.whatismyip.com/n09230945.asp` 
    echo $ip | pbcopy
    echo $ip
}

