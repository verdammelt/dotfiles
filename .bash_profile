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

complete -o bashdefault -o default -o nospace -F _git g 2> /dev/null \
    || complete -o default -o nospace -F _git g


function project_dispatcher() {
    ~/Bin/`basename $PWD` $*
}

for c in build full deploy start stop; do
    alias $c="project_dispatcher $c"
done
