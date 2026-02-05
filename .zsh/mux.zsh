# Unified multiplexer abstraction layer
# Automatically detects screen or tmux and provides unified interface

function _mux_detect () {
    if [[ -n "$TMUX" ]]; then
        echo "tmux"
    elif [[ -n "$STY" ]]; then
        echo "screen"
    else
        echo ""
    fi
}

function _mux_default () {
    # Check user preference
    [[ -n "$MUX_PREFER" ]] && { echo "$MUX_PREFER"; return }

    # Default to tmux if available, fallback to screen
    if command -v tmux >/dev/null 2>&1; then
        echo "tmux"
    elif command -v screen >/dev/null 2>&1; then
        echo "screen"
    else
        echo ""
    fi
}

function mux_type () {
    local detected; detected=`_mux_detect`
    [[ -n "$detected" ]] && echo "$detected" || _mux_default
}

function mux_session_id () {
    case "`_mux_detect`" in
        tmux)
            tmux display-message -p '#S' 2>/dev/null
            ;;
        screen)
            echo "$STY"
            ;;
        *)
            echo ""
            ;;
    esac
}

function mux_window_id () {
    case "`_mux_detect`" in
        tmux)
            tmux display-message -p '#I' 2>/dev/null
            ;;
        screen)
            echo "$WINDOW"
            ;;
        *)
            echo ""
            ;;
    esac
}

function mux_getenv () {
    local session="${1:-$(mux_session_id)}"
    local evar="$2"
    (( $# >= 2 )) && shift 2

    case "`mux_type`" in
        tmux)
            tmux_getenv "$session" "$evar"
            ;;
        screen)
            screen_getenv "$session" "$evar"
            ;;
    esac
}

function mux_setenv () {
    local session="${1:-$(mux_session_id)}"
    local evar="$2"
    local value="$3"
    (( $# >= 3 )) && shift 3

    case "`mux_type`" in
        tmux)
            tmux_setenv "$session" "$evar" "$value"
            ;;
        screen)
            screen_setenv "$session" "$evar" "$value"
            ;;
    esac
}

function mux_attach () {
    case "`mux_type`" in
        tmux)
            tmux_attach "$@"
            ;;
        screen)
            screen_attach "$@"
            ;;
        *)
            echo "No multiplexer available" > /dev/stderr
            return 1
            ;;
    esac
}

function mux_list () {
    case "`mux_type`" in
        tmux)
            tmux_list
            ;;
        screen)
            screen_list
            ;;
    esac
}

# Export multiplexer type
export MUX_TYPE=`mux_type`
