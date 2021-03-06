function screen_debug_log () {
    [[ -n "$SCREEN_DEBUG_LOG" ]] && {
        local date; date=`date --iso=seconds`
        echo "$date $1" >> "$SCREEN_DEBUG_LOG"
    }
}

function screen_list () {
    zmodload zsh/regex
    local line
    for line in ${(f)"$(screen -ls)"}; do
        [[ "$line" -regex-match '\s*(\S+)\s+\((.*+)\)' ]] && {
            st=(${(ps:, :)match[2]})
            echo "$match[1] ($st)"
        }
    done
}
function screen_list_filter () {
    zmodload zsh/regex
    local filter; filter="$1"; local line
    for line in ${(f)"$(screen_list)"}; do
        [[ "$line" -regex-match "$1" ]] && echo "$line"
    done
}
function screen_list_filter_status () {
    zmodload zsh/regex
    emulate -L zsh
    unsetopt case_match
    local filter; filter="\\((\\S+ )*$1( \\S+)*\\)$"; local line
    for line in ${(f)"$(screen_list_filter "$filter")"}; do
        [[ "$line" -regex-match '^(.*) \(.*\)$' ]] && echo "$match[1]"
    done
}
function screen_list_attached () {
    screen_list_filter_status attached
}
function screen_list_detached () {
    screen_list_filter_status detached
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
        screen_debug_log "[$sty] unsetenv $1"
    else
        2="${2//\'/\\\'}"; 2="${2//\"/\\\\\"}"
        screen -S "$sty" -X eval "setenv $1 \"$2\""
        screen_debug_log "[$sty] setenv $1 \"$2\""
    fi
}
