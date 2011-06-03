# set window title of screen
function _screen_set_title () { echo -ne "\ek$1\e\\" }
function _screen_title_install_precmd () { # use current directory as a title
    function precmd_screen_window_title () {
        if [[ "$SCREEN_TITLE" = 'auto' ]]; then
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
            _screen_set_title "$dir"
        fi
    }
}
_screen_title_install_precmd
typeset -A SCREEN_TITLE_ARG; SCREEN_TITLE_ARG=()
typeset -A SCREEN_TITLE_CMD_ARG; SCREEN_TITLE_CMD_ARG=(ssh -1 su -1 man -1)
typeset -A SCREEN_TITLE_CMD_IGNORE; SCREEN_TITLE_CMD_IGNORE=()
function _screen_title_install_preexec { # use command name as a title
    function _screen_set_cmd_title () {
        zmodload zsh/regex
        local -a cmd; cmd=(${(z)1}); local prefix; local title
        while [[ "$cmd[1]" -regex-match "[^\\]=" ]]; do shift cmd; done
        [[ "$cmd[1]" == "command" ]] && shift cmd
        [[ "$cmd[1]" == "builtin" ]] && shift cmd
        [[ "${cmd[1]:t}" == "env" ]] && shift cmd
        [[ "$cmd[1]" == "sudo" ]] && shift cmd && prefix='#'
        if [[ -n "$SCREEN_TITLE_CMD_IGNORE[${cmd[1]:t}]" ]]; then
            return
        elif [[ -n "$SCREEN_TITLE_ARG[${cmd[1]:t}]" ]]; then
            # argument of command
            title="${cmd[$SCREEN_TITLE_ARG[${cmd[1]:t}]]:t}"
        elif [[ -n "$SCREEN_TITLE_CMD_ARG[${cmd[1]:t}]" ]]; then
            # command itself and argument of command
            title="${cmd[$SCREEN_TITLE_CMD_ARG[${cmd[1]:t}]]:t}"
            [[ $#cmd > 1 || -z "$title" ]] && {
                prefix+="${cmd[1]:t}"
                [[ -n "$title" ]] && prefix+=':'
            }
        else
            title="${cmd[1]:t}"
        fi
        _screen_set_title "$prefix$title"
    }
    function preexec_screen_window_title () {
        local -a cmd; cmd=(${(z)2}) # command in a single line
        if [[ "$SCREEN_TITLE" = 'auto' ]]; then
            case $cmd[1] in
                fg)
                    if (( $#cmd == 1 )); then
                        cmd=(builtin jobs -l %+)
                    else
                        cmd=(builtin jobs -l $cmd[2])
                    fi
                    ;;
                %*)
                    cmd=(builtin jobs -l $cmd[1])
                    ;;
                *)
                    _screen_set_cmd_title "$cmd"
                    return
                    ;;
            esac
            # resolve command in jobs
            local -A jt; jt=(${(kv)jobtexts})
            $cmd >>(read num rest
                cmd=(${(z)${(e):-\$jt$num}})
                _screen_set_cmd_title "$cmd"
            ) 2>/dev/null
        fi
    }
}
_screen_title_install_preexec
function screen_title() {
    if [[ -n "$SCREEN_TITLE" ]]; then
        if [[ -n "$1" ]]; then
            # set title explicitly
            SCREEN_TITLE=explicit
            _screen_set_title "$1"
        else
            # automatically set title
            SCREEN_TITLE=auto
        fi
    fi
}

[[ -n "$STY" ]] && {
    precmd_functions+=precmd_screen_window_title
    preexec_functions+=preexec_screen_window_title
}
