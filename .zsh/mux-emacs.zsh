# Multiplexer-aware Emacs daemon registration

function _mux_emacs_register () {
    local mux; mux=`mux_type`
    [[ -z "$mux" ]] && return

    local session; session=`mux_session_id`
    local comm; comm=`_emacs_get_comm $@`
    [[ -z "$comm" ]] && comm='default'

    # Get registered daemons
    local reg; reg=`mux_getenv "$session" "${mux^^}_EMACSD"`
    reg=(${(s.:.)reg})

    # Register emacs daemon
    local num; num=$reg[(i)$comm]
    reg[$num]="$comm"
    mux_setenv "$session" "${mux^^}_EMACSD" "${(j.:.)reg}"

    # Create attach hook
    local hook_var="${mux^^}_EMACSD_ENV$num"
    local hook_cmd
    case "$mux" in
        tmux)
            hook_cmd="emacsclient $@ -e '(tmux-sync-env \"$session\")'"
            tmux_add_attach_hook "$session" "$hook_var" "$hook_cmd"
            ;;
        screen)
            hook_cmd="emacsclient $@ -e '(screen-sync-env \"$session\")'"
            screen_add_attach_hook "$session" "$hook_var" "$hook_cmd"
            ;;
    esac
}

# Hook into emacs function only if in a multiplexer
if [[ -n "`_mux_detect`" ]]; then
    # Save original emacs function
    functions[_mux_emacs_original]="${functions[emacs]}"

    function emacs () {
        if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
            emacs-standalone "$@"
        else
            emacsd status >/dev/null || emacsd start
            _mux_emacs_register "$@"
            DISPLAY="$DISPLAY" emacsc "$@"
        fi
    }
fi
