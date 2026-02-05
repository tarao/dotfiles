function tmux_debug_log () {
    [[ -n "$TMUX_DEBUG_LOG" ]] && {
        local date; date=`date --iso=seconds`
        echo "$date $1" >> "$TMUX_DEBUG_LOG"
    }
}

function tmux_list () {
    tmux list-sessions 2>/dev/null | while IFS=: read -r name rest; do
        echo "$name ($rest)"
    done
}

function tmux_list_filter_status () {
    local filter_status="$1"
    tmux list-sessions -F "#{session_name} #{session_attached}" 2>/dev/null | \
    while read -r name attached; do
        case "$filter_status" in
            attached)
                [[ "$attached" == "1" ]] && echo "$name"
                ;;
            detached)
                [[ "$attached" == "0" ]] && echo "$name"
                ;;
        esac
    done
}

function tmux_list_attached () { tmux_list_filter_status attached }
function tmux_list_detached () { tmux_list_filter_status detached }

function tmux_getenv () {
    local session; session="$(tmux display-message -p '#S' 2>/dev/null)"
    local evar
    (( $# >= 2 )) && { session="$1"; shift }
    evar="$1"
    tmux show-environment -t "$session" "$evar" 2>/dev/null | cut -d= -f2-
}

function tmux_setenv () {
    local session; session="$(tmux display-message -p '#S' 2>/dev/null)"
    local evar
    (( $# >= 3 )) && { session="$1"; shift }
    if [[ -z "$2" ]]; then
        tmux set-environment -t "$session" -u "$1"
        tmux_debug_log "[$session] unset $1"
    else
        tmux set-environment -t "$session" "$1" "$2"
        tmux_debug_log "[$session] setenv $1 \"$2\""
    fi
}
