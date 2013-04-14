[[ -s "/Users/mark/.rvm/scripts/rvm" ]] && source "/Users/mark/.rvm/scripts/rvm"
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion
[[ -f "$HOME/.githubtoken" ]] && . $HOME/.githubtoken

export CLICOLOR=1

function g {
  if [[ $# > 0 ]]; then
    git $@
  else
    git status
  fi
}
__git_complete g _git

#alias g='git'
alias ga='git add'
alias gd='git di'
alias gci='git ci'
alias df='df -h'
alias ls="ls -F"
alias rm="rm -i"
alias fat="(cd ~/SRC/thefattrack && rails s)"
alias git=hub
alias pdfopen='pdfopen -viewer xpdf'
alias l=ls
alias ll="ls -l"
alias v=vi

PATH=$HOME/Bin:$HOME/.rvm/bin:/usr/local/bin:$PATH
NODE_PATH=/usr/local/lib/node_modules
MANPATH=$MANPATH:/opt/local/man
TNEFSUBREP=https://tnef.svn.sourceforge.net/svnroot/tnef/
EDITOR=vim
VISUAL=vim
export LLPROGRAMS=/usr/local/share/lifelines
export LLREPORTS=$HOME/LifeLines/output
export LLARCHIVE=$HOME/LifeLines/archive
export LLDATABASES=$HOME/LifeLines

export AUTOFEATURE=true

alias tnefrsync="rsync -av tnef.svn.sourceforge.net::svn/tnef/* ."

alias llines='(cd $HOME/LifeLines ; llines simpson)'

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWUNTRACKEDFILES=1

# turning of erase dups because i want to use huffshell to suggest aliases
#export HISTCONTROL=erasedups
export HISTSIZE=5000

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

