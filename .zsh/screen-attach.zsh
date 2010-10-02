SCREEN_EXPORT_ENV=(DISPLAY XAUTHORITY SSH_CONNECTION SSH_CLIENT SSH_TTY)
function _screen_export_env () {
    local e; local sty; sty="$1"
    [[ -z "$sty" ]] && return
    foreach e in ${SCREEN_EXPORT_ENV}
        screen -S "$sty" -X eval "setenv ${e} '${(P)e}'"
    end
}
function _screen_import_env () {
    local e; local evar; local val
    foreach e in ${SCREEN_EXPORT_ENV}
        evar='${'"$e"'}'
        val=`screen -S "$STY" -Q echo "$evar"`
        if [[ $? == 0 ]] && [[ -n "${val}" ]]; then
            export ${e}="${val}"
        else
            unset ${e}
        fi
    end
}
function screen_auto_env () {
    [[ -z "$1" ]] && 1=on
    case "$1" in
        on|1|true)
            unset _screen_no_auto_env
            ;;
        off|0|false)
            _screen_no_auto_env=1
            ;;
    esac
}
[[ -n "$STY" ]] && {
    function preexec_screen_import_env () {
        [[ -z "$_screen_no_auto_env" ]] && _screen_import_env
    }
    preexec_functions+=preexec_screen_import_env
}

function screen_attach () {
    [[ -n "$STY" ]] && {
        echo "$0: already attached"
        return
    }
    local sty; sty="$1"
    [[ -z "$sty" ]] && {
        # find detached screen or select first one
        local attached; local detached; local k; local st
        attached=(); detached=()
        screen_list > /dev/null
        foreach k in ${(k)_screen_list}
            st=(${(z)_screen_list[$k]})
            [[ -n "$st[(r)attached]" ]] && attached=($attached $k)
            [[ -n "$st[(r)detached]" ]] && detached=($detached $k)
        end
        (( ${#attached} > 0 )) && sty="$attached[1]"
        (( ${#detached} > 0 )) && sty="$detached[1]"
        [[ -z "$sty" ]] && {
            # nothing to attach; create new session
            screen
            return
        }
    }
    ( sleep 1; _screen_export_env "$sty" ) &! screen -x -r "$sty"
}
function screen_detach () {
    [[ -n "$STY" ]] && screen -d "$STY" > /dev/null
}
