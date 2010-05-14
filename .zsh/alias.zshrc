alias ls='/bin/ls -F $=LS_OPTIONS'
alias dir='ls -l'
alias la='ls -la'
alias ll='ls -alF'
alias ..='cd ..'
alias ...='cd ../..'
alias -- +='pushd .'
alias -- -='popd'

alias sc='screen -h 4096'
alias wcat='wget -q -O -'

alias man='LANG=${LANG/en_US.UTF-8/en_US} env man'
alias diff='colordiff -u'
alias od='od -A x -t xCz'
alias apt='LANG=C aptitude'
alias sapt='LANG=C sudo aptitude'

function { # local scope
    local w
    w="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
    alias c++="env g++ -lstdc++ -std=c++98 -pedantic-errors $w"
    alias c++now="env g++ -lstdc++ -std=c++98 -pedantic-errors"
    alias g++="env g++ -lstdc++ -std=gnu++98 -pedantic $w"
    alias g++now="env g++ -lstdc++ -std=gnu++98 -pedantic"
}
