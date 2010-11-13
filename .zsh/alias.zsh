alias ls='command ls -F $=LS_OPTIONS'
alias dir='ls -l'
alias la='ls -la'
alias ll='ls -alF'
alias ..='cd ..'
alias ...='cd ../..'
alias -- +='pushd .'
alias -- -='popd'

alias fwd='ssh -N -F ~/.ssh/fwd_config -N'
alias proxy='ssh -F ~/.ssh/proxy_config -N'

alias man='LANG=${LANG/en_US.UTF-8/en_US} command man'
alias od='od -A x -t xCz'
alias apt='LANG=C aptitude'
alias sapt='LANG=C sudo aptitude'
alias wcat='wget -q -O -'

function grep () {
    if env test -t 0; then
        # stdin is a terminal
        command grep -nH --color $@
    else
        # stdin is a pipe
        command grep $@
    fi
}
function diff () {
    colordiff -u $@ | $PAGER
}
function last () {
    command last $@ | $PAGER
}

function { # local scope
    local w
    w="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
    alias c++="command g++ -lstdc++ -std=c++98 -pedantic-errors $w"
    alias c++now="command g++ -lstdc++ -std=c++98 -pedantic-errors"
    alias g++="command g++ -lstdc++ -std=gnu++98 -pedantic $w"
    alias g++now="command g++ -lstdc++ -std=gnu++98 -pedantic"
}
