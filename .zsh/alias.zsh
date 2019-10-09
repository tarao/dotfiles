function ls () {
    if test -t 1; then
        # stdout is a terminal
        command ls -F $=LS_OPTIONS $@
    else
        # stdout is a pipe
        command ls $=LS_OPTIONS $@
    fi
}

whence xdg-open > /dev/null && {
    function xdg_open () {
        command xdg-open "$@" >/dev/null 2>/dev/null
    }
    alias ii='xdg_open'
}
alias dir='ls -l'
alias la='ls -la'
alias ll='ls -alF'
alias ..='cd ..'
alias ...='cd ../..'
alias -- +='pushd .'
alias -- -='popd'

alias fwd='ssh -N -F ~/.ssh/fwd_config -N'
alias proxy='ssh -F ~/.ssh/proxy_config -N'

alias od='od -A x -t xCz'
alias apt='LANG=C aptitude'
alias sapt='LANG=C sudo aptitude'
alias wcat='wget -q -O -'
alias whead='wget -S -O /dev/null'
function wexif() {
    exif <(wget --no-check-certificate -O - "$@" 2>/dev/null)
}
alias xsel='xsel -b'

alias ce='carton exec --'
alias be='bundle exec --'

alias e='emacs-edit -n'

function ssh () {
    local cmd; cmd=(command ssh)
    whence zssh >/dev/null && cmd=(zssh -z \^\] --)
    $cmd "$@"
}
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
    whence colordiff >/dev/null && [ -n "$PAGER" ] && {
        colordiff -u "$@" | ${=PAGER}
    } || command diff "$@"
}
function last () {
    [ -n "$PAGER" ] && {
        command last "$@" | ${=PAGER}
    } || command last "$@"
}
function man () {
    local which=$(which "$1")
    if echo "$which" | grep "$1: shell built-in command" >/dev/null 2>&1; then
        LANG=C command man -P "less -p'^       $1 '" zshbuiltins
    else
        LANG=C command man "$@"
    fi
}

function { # local scope
    local w
    w="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
    alias c++="command g++ -lstdc++ -std=c++98 -pedantic-errors $w"
    alias c++now="command g++ -lstdc++ -std=c++98 -pedantic-errors"
    alias g++="command g++ -lstdc++ -std=gnu++98 -pedantic $w"
    alias g++now="command g++ -lstdc++ -std=gnu++98 -pedantic"
}

whence rlwrap >/dev/null && {
    alias tinyrepl="rlwrap tinyrepl"
}
