# Screen settings are reused
TMUX_CRITICAL_ENV=(${SCREEN_CRITICAL_ENV})
TMUX_EXPORT_ENV=(${SCREEN_EXPORT_ENV})

function _tmux_make_env_id () {
    local id; local ids; ids=()
    for id in ${TMUX_CRITICAL_ENV}; do ids+="${(P)id}"; done
    echo "${(j: - :)ids}"
}

function _tmux_export_env () {
    local session; session="$1"
    [[ -z "$session" ]] && return

    local e
    for e in ${TMUX_EXPORT_ENV}; do
        tmux_setenv "$session" "$e" "${(P)e}"
    done

    # Run hooks
    local tmux_attach_hook; local hook_name; local hook_val
    tmux_attach_hook=`tmux_getenv "$session" TMUX_ATTACH_HOOK`
    for hook_name in ${(z)tmux_attach_hook}; do
        hook_val=`tmux_getenv "$session" "$hook_name"`
        [[ -n "${hook_val}" ]] && {
            eval "${hook_val} >/dev/null 2>&1"
            (( $? != 0 )) && tmux_remove_attach_hook "$session" "$hook_name"
        }
    done

    local id; id=`_tmux_make_env_id`
    tmux_setenv "$session" TMUX_ENV_ID "$id"
}

function __tmux_import_env () {
    local e; local val
    local session; session="$(tmux display-message -p '#S')"
    local imports; imports=(${TMUX_EXPORT_ENV} TMUX_ENV_ID)
    for e in $imports; do
        val=`tmux_getenv "$session" "$e"`
        if [[ $? == 0 ]] && [[ -n "${val}" ]]; then
            export ${e}="${val}"
        else
            unset ${e}
        fi
    done
}

function _tmux_import_env () {
    local session; session="$(tmux display-message -p '#S')"
    local id; id=`tmux_getenv "$session" TMUX_ENV_ID`
    [[ "$id" == "$TMUX_ENV_ID" ]] && return
    __tmux_import_env
}

function tmux_auto_env () {
    case "$1" in
        on|1|true) unset _tmux_no_auto_env ;;
        off|0|false) _tmux_no_auto_env=1 ;;
        *)
            if [[ -z "$_tmux_no_auto_env" ]]; then
                echo on
            else
                echo off
            fi
            ;;
    esac
}

[[ -n "$TMUX" ]] && {
    function preexec_tmux_import_env () {
        [[ -z "$_tmux_no_auto_env" ]] && _tmux_import_env
    }
    preexec_functions+=preexec_tmux_import_env
}

function tmux_attach () {
    [[ -n "$TMUX" ]] && {
        echo "$0: already attached" > /dev/stderr
        return
    }
    local session; session="$1"
    [[ -z "$session" ]] && {
        local attached; local detached
        attached=(${(f)"$(tmux_list_attached)"%\n})
        detached=(${(f)"$(tmux_list_detached)"%\n})
        [[ ${#attached} > 0 ]] && session="$attached[1]"
        [[ ${#detached} > 0 ]] && session="$detached[1]"
        [[ -z "$session" ]] && {
            tmux
            return
        }
    }
    _tmux_export_env "$session"
    tmux attach-session -t "$session"
}

function tmux_detach () {
    [[ -n "$TMUX" ]] && tmux detach-client
}

function tmux_add_attach_hook () {
    local session; session="$(tmux display-message -p '#S' 2>/dev/null)"
    local hooks
    (( $# >= 3 )) && { session="$1"; shift }
    hooks=`tmux_getenv "$session" TMUX_ATTACH_HOOK`; hooks=(${(z)hooks})
    if [[ -z "$2" ]]; then
        hooks[(i)$1]=()
    else
        hooks[(i)$1]="$1"
    fi
    tmux_setenv "$session" TMUX_ATTACH_HOOK "$hooks"
    tmux_setenv "$session" "$1" "$2"
}

function tmux_remove_attach_hook () {
    tmux_add_attach_hook $@ ""
}
