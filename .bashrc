[[ -s "/Users/mark/.rvm/scripts/rvm" ]] && source "/Users/mark/.rvm/scripts/rvm"
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion

export CLICOLOR=1
alias ls="ls -F"
alias rm="rm -i"
alias fat="(cd ~/SRC/thefattrack && rails s)"

PATH=$HOME/Bin:$HOME/.rvm/bin:/opt/local/libexec/gnubin:/opt/local/bin:/usr/local/bin:$PATH

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

alias llines='(cd $HOME/LifeLines ; /opt/local/bin/llines simpson)'

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

function min_since_commit() {
    min=`git_minutes_since_last_commit`
    if [ "$min" -eq -1 ]; then
        echo -n ""
    else
        if [ "$min" -gt 720 ]; then
            color="$red"
        elif [ "$min" -gt 120 ]; then
            color=$yellow
        elif [ "$min" -gt 60 ]; then
            color=$purple
        elif [ "$min" -gt 30 ]; then
            color=$cyan
        else
            color=$green
        fi
        echo -e $color"${min}m"$reset
    fi
}

function gitPS1() {
    if [ $(__gitdir) ]; then
#	prompt='('$(min_since_commit)'|'$purple$(__git_ps1 "%s")$reset')'
prompt="\\[\\033[0;33m\\]$(__git_ps1)\\[\\033[0m\\]"
    else
        prompt=""
    fi
    echo $prompt
}

export PS1='\h:\W'$purple'$(__git_ps1 "(%s)")'$reset'> '

alias git="git-achievements"

function myip(){ 
    ip=`curl -s automation.whatismyip.com/n09230945.asp` 
    echo $ip | pbcopy
    echo $ip
}

