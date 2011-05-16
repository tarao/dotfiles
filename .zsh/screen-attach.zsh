SCREEN_CRITICAL_ENV=(DISPLAY SSH_CONNECTION SSH_AUTH_SOCK)
SCREEN_EXPORT_ENV=(
    DISPLAY XAUTHORITY
    SSH_CONNECTION SSH_CLIENT SSH_TTY SSH_AUTH_SOCK
)

function _screen_make_env_id () {
    local id; local ids; ids=()
    for id in ${SCREEN_CRITICAL_ENV}; do ids+="${(P)id}"; done
    echo "${(j: - :)ids}"
}
function _screen_export_env () {
    # set environment variables
    local e; local sty; sty="$1"
    [[ -z "$sty" ]] && return
    for e in ${SCREEN_EXPORT_ENV}; do screen_setenv "$sty" "$e" "${(P)e}"; done

    # run hooks
    local screen_attach_hook; local hook_name; local hook_val
    screen_attach_hook=`screen_getenv "$sty" SCREEN_ATTACH_HOOK`
    for hook_name in ${(z)screen_attach_hook}; do
        hook_val=`screen_getenv "$sty" "$hook_name"`
        [[ -n "${hook_val}" ]] && {
            eval "${hook_val} >/dev/null 2>&1"
            (( $? != 0 )) && screen_remove_attach_hook "$sty" "$hook_name"
        }
    done

    # make new ID for the environment
    local id; id=`_screen_make_env_id`
    screen_setenv "$sty" SCREEN_ENV_ID "$id"
}
function __screen_import_env () {
    local e; local evar; local val
    local imports; imports=(${SCREEN_EXPORT_ENV} SCREEN_ENV_ID)
    for e in $imports; do
        val=`screen_getenv "$STY" "$e"`
        if [[ $? == 0 ]] && [[ -n "${val}" ]]; then
            export ${e}="${val}"
        else
            unset ${e}
        fi
    done
}
function _screen_import_env () {
    local id; id=`screen_getenv "$STY" SCREEN_ENV_ID`
    [[ "$id" == "$SCREEN_ENV_ID" ]] && return # no change in the environment
    __screen_import_env
}

function screen_auto_env () {
    case "$1" in
        on|1|true)
            unset _screen_no_auto_env
            ;;
        off|0|false)
            _screen_no_auto_env=1
            ;;
        *)
            if [[ -z "$_screen_no_auto_env" ]]; then
                echo on
            else
                echo off
            fi
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
        echo "$0: already attached" > /dev/stderr
        return
    }
    local sty; sty="$1"
    [[ -z "$sty" ]] && {
        # find detached screen or select first one
        local attached; local detached; local k; local st
        attached=${(f)$(screen_list_attached)}
        detached=${(f)$(screen_list_detached)}
        (( ${#attached} > 0 )) && sty="$attached[1]"
        (( ${#detached} > 0 )) && sty="$detached[1]"
        [[ -z "$sty" ]] && {
            # nothing to attach; create new session
            screen
            return
        }
    }
    _screen_export_env "$sty"
    screen -x -r "$sty"
}
function screen_detach () {
    [[ -n "$STY" ]] && screen -d "$STY" > /dev/null
}
function screen_add_attach_hook () {
    local sty; sty="$STY"; local hooks
    (( $# >= 3 )) && { sty="$1"; shift }
    hooks=`screen_getenv "$sty" SCREEN_ATTACH_HOOK`; hooks=(${(z)hooks})
    if [[ -z "$2" ]]; then
        hooks[(i)$1]=()   # remove
    else
        hooks[(i)$1]="$1" # add or replace
    fi
    screen_setenv "$sty" SCREEN_ATTACH_HOOK "$hooks"
    screen_setenv "$sty" "$1" "$2"
}
function screen_remove_attach_hook () {
    screen_add_attach_hook $@ ""
}
