function tm () {
    if [[ -z "$1" ]]; then
        tmux
    else
        local action; action="$1"; shift
        local cmd; cmd="tmux_${action/-/_}"
        if [[ "${+functions[$cmd]}" == 1 ]]; then
            $cmd $@
        else
            echo "$0: action '$action' not found" > /dev/stderr
        fi
    fi
}
alias tma='tm attach'
alias tmd='tm detach'
