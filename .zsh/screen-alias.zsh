function sc () {
    if [[ -z "$1" ]]; then
        screen
    else
        local action; action="$1"; shift
        local cmd; cmd="screen_${action/-/_}"
        if [[ "${+functions[$cmd]}" == 1 ]]; then
            $cmd $@
        else
            echo "$0: action '$action' not found" > /dev/stderr
        fi
    fi
}
alias sca='sc attach'
alias scd='sc detach'
