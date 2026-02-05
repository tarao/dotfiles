# set window title of tmux
function _tmux_set_title () {
    [[ -n "$TMUX" ]] && tmux rename-window "$1"
}

function _tmux_title_install_precmd () { # use current directory as a title
    function precmd_tmux_window_title () {
        if [[ "$TMUX_TITLE" = 'auto' ]]; then
            local dir
            dir=`pwd`
            dir=`print -nD "$dir"`
            if [[ ( -n "$vcs" ) && ( "$repos" != "$dir" ) ]]; then
                # name of repository and directory
                dir="${repos:t}:${dir:t}"
            elif [[ "$dir" != "/" ]]; then
                # name of directory
                dir=${dir:t}
            fi
            _tmux_set_title "$dir"
        fi
    }
}
_tmux_title_install_precmd

typeset -A TMUX_TITLE_ARG; TMUX_TITLE_ARG=(${(kv)SCREEN_TITLE_ARG})
typeset -A TMUX_TITLE_CMD_ARG; TMUX_TITLE_CMD_ARG=(${(kv)SCREEN_TITLE_CMD_ARG})
typeset -A TMUX_TITLE_CMD_IGNORE; TMUX_TITLE_CMD_IGNORE=(${(kv)SCREEN_TITLE_CMD_IGNORE})

function _tmux_title_install_preexec { # use command name as a title
    function _tmux_set_cmd_title () {
        zmodload zsh/regex
        local -a cmd; cmd=(${(z)1}); local prefix; local title
        while [[ "$cmd[1]" -regex-match "[^\\]=" ]]; do shift cmd; done
        [[ "$cmd[1]" == "command" ]] && shift cmd
        [[ "$cmd[1]" == "builtin" ]] && shift cmd
        [[ "${cmd[1]:t}" == "env" ]] && shift cmd
        [[ "$cmd[1]" == "sudo" ]] && shift cmd && prefix='#'
        if [[ -n "$TMUX_TITLE_CMD_IGNORE[${cmd[1]:t}]" ]]; then
            return
        elif [[ -n "$TMUX_TITLE_ARG[${cmd[1]:t}]" ]]; then
            # argument of command
            title="${cmd[$TMUX_TITLE_ARG[${cmd[1]:t}]]:t}"
        elif [[ -n "$TMUX_TITLE_CMD_ARG[${cmd[1]:t}]" ]]; then
            # command itself and argument of command
            title="${cmd[$TMUX_TITLE_CMD_ARG[${cmd[1]:t}]]:t}"
            [[ $#cmd > 1 || -z "$title" ]] && {
                prefix+="${cmd[1]:t}"
                [[ -n "$title" ]] && prefix+=':'
            }
        else
            title="${cmd[1]:t}"
        fi
        _tmux_set_title "$prefix$title"
    }
    function preexec_tmux_window_title () {
        local -a cmd; cmd=(${(z)2}) # command in a single line
        if [[ "$TMUX_TITLE" = 'auto' ]]; then
            case "$cmd[1]" in
                fg)
                    if (( $#cmd == 1 )); then
                        cmd=(builtin jobs -l %+)
                    else
                        cmd=(builtin jobs -l "$cmd[2]")
                    fi
                    ;;
                %*)
                    cmd=(builtin jobs -l "$cmd[1]")
                    ;;
                *)
                    _tmux_set_cmd_title "$cmd"
                    return
                    ;;
            esac
            # resolve command in jobs
            local -A jt; jt=(${(kv)jobtexts})
            $cmd >>(read num rest
                cmd=(${(z)${(e):-\$jt$num}})
                _tmux_set_cmd_title "$cmd"
            ) 2>/dev/null
        fi
    }
}
_tmux_title_install_preexec

function tmux_title() {
    if [[ -n "$TMUX_TITLE" ]]; then
        if [[ -n "$1" ]]; then
            # set title explicitly
            TMUX_TITLE=explicit
            _tmux_set_title "$1"
        else
            # automatically set title
            TMUX_TITLE=auto
        fi
    fi
}

[[ -n "$TMUX" ]] && {
    export TMUX_TITLE=auto
    precmd_functions+=precmd_tmux_window_title
    preexec_functions+=preexec_tmux_window_title
}
