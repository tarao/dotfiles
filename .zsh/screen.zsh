typeset -A _screen_list
function screen_list () {
    _screen_list=()
    local ls; ls=`screen -ls`; local line
    foreach line in ${(f)ls}
        [[ "$line" =~ '\s*(\S+)\s+\((.*+)\)' ]] && {
            st=(${(ps:, :)match[2]})
            _screen_list[$match[1]]="$st"
            echo "$match[1] ($st)"
        }
    end
}
function screen_getenv () {
    local sty; sty="$STY"; local evar
    (( $# >= 2 )) && { sty="$1"; shift }; evar='${'"$1"'}'
    screen -S "$sty" -Q echo "$evar"
}
function screen_setenv () {
    local sty; sty="$STY"; local evar
    (( $# >= 3 )) && { sty="$1"; shift }
    if [[ -z "$2" ]]; then
        screen -S "$sty" -X eval "unsetenv $1"
    else
        screen -S "$sty" -X eval "setenv $1 '$2'"
    fi
}
