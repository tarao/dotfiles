# require term+
[[ `echo $INSIDE_EMACS | cut -f 2 -d ,` = term:*+ ]] || return

# send OSC 51 ; Ps ; Pd ST commans
function osc_emacs () {
    echo -ne "\e]51;$1;$2\e\\"
}
# send OSC 52 ; Pc ; Pd ST commans
function osc_sel () {
    echo -ne "\e]52;$1;$2\e\\"
}

# set current directory
function precmd_eterm_cwd () {
    local dir; dir=`pwd`
    osc_emacs 'd' "$dir"
}
precmd_functions+=precmd_eterm_cwd

# set host and user
function eterm_current_host () {
    local host; host=`hostname`
    osc_emacs 'h' "$host"
}
function eterm_current_user () {
    local user; user=`id -run`
    osc_emacs 'u' "$user"
}

eterm_current_host
eterm_current_user

# switch to term-line-mode
function switch-to-line-mode () {
    local rprompt="$RPROMPT"
    RPROMPT=''
    local buf="$BUFFER"
    zle kill-buffer
    zle reset-prompt
    zle -R
    osc_emacs 'm' 'sh-mode' # TODO: check 'mode' flag in $INSIDE_EMACS
    osc_sel "$1" "$buf"
    RPROMPT="$rprompt"
}
function switch-to-line-mode-normal () {
    switch-to-line-mode 'n'
}
function switch-to-line-mode-insert () {
    switch-to-line-mode 'i'
}

# TODO: check 'evil' flag in $INSIDE_EMACS
zle -N switch-to-line-mode-normal
zle -N switch-to-line-mode-insert

bindkey '^[' switch-to-line-mode-normal
bindkey '^[i' switch-to-line-mode-insert

function o () {
    [[ "$1" == '-h' || "$1" == '--help' ]] && {
        echo "Usage: $0 file..."
        return
    }

    local -a inputs; inputs=()
    local rest
    for f in $@; do
        [[ -n "$rest" || "$f" != '-'* ]] && { inputs=("${(@)inputs}" $f) }
        [[ "$f" == '--' ]] && { rest='true' }
    done

    if (( $#inputs > 0 )) && [[ -t 0 && -t 1 ]]; then
        # there are inputs and no piped output
        osc_sel 'o' "${(j:;:)inputs}"
        return
    fi

    # piped or not in Emacs
    if type -p "$0" >/dev/null; then
        command "$0" "$@"
    elif [[ -n "$EDITOR" ]] && type "$EDITOR" >/dev/null; then
        "$EDITOR" "$@"
    elif type vi >/dev/null; then
        vi "$@"
    elif type ed >/dev/null; then
        ed "$@"
    else
        echo 'No editor'
    fi
}

function v () {
    [[ "$1" == '-h' || "$1" == '--help' ]] && {
        echo "Usage: $0 file..."
        return
    }

    local -a inputs; inputs=()
    local rest
    for f in $@; do
        [[ -n "$rest" || "$f" != '-'* ]] && { inputs=("${(@)inputs}" $f) }
        [[ "$f" == '--' ]] && { rest='true' }
    done

    if (( $#inputs > 0 )) && [[ -t 0 && -t 1 ]]; then
        # there are inputs and no piped output
        osc_sel 'v' "${(j:;:)inputs}"
        return
    fi

    # piped or not in Emacs
    if type -p "$0" >/dev/null; then
        command "$0" "$@"
    elif type less >/dev/null; then
        less "$@"
    elif type cat >/dev/null; then
        cat "$@"
    else
        echo 'No viewer'
    fi
}
