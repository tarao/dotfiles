alias ls='/bin/ls -F $=LS_OPTIONS'
alias dir='ls -l'
alias la='ls -la'
alias ll='ls -alF'
alias ..='cd ..'
alias ...='cd ../..'
alias -- +='pushd .'
alias -- -='popd'

alias sc='screen'
function sca () {
    [[ -z "$STY" ]] && screen -xR
}
function scd () {
    [[ -n "$STY" ]] && screen -d $STY >/dev/null
}

alias fwd='ssh -N -F ~/.ssh/fwd_config'
alias proxy='ssh -F ~/.ssh/proxy_config'

alias man='LANG=${LANG/en_US.UTF-8/en_US} env man'
alias od='od -A x -t xCz'
alias apt='LANG=C aptitude'
alias sapt='LANG=C sudo aptitude'
alias wcat='wget -q -O -'

function diff () {
    colordiff -u $@ | v
}
alias last='env last | v'

function { # local scope
    local w
    w="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
    alias c++="env g++ -lstdc++ -std=c++98 -pedantic-errors $w"
    alias c++now="env g++ -lstdc++ -std=c++98 -pedantic-errors"
    alias g++="env g++ -lstdc++ -std=gnu++98 -pedantic $w"
    alias g++now="env g++ -lstdc++ -std=gnu++98 -pedantic"
}
