# set window title of screen
function _screen_set_title () { echo -ne "\ek$1\e\\" }
function { # use current directory as a title
    function precmd_screen_window_title () {
        if [[ "$SCREENTITLE" = 'auto' ]]; then
            local dir
            dir=`pwd`
            dir=`print -nD "$dir"`
            if [[ ( -n "$vcs" ) && ( "$repos" != "$dir" ) ]]; then
                # name of repository and directory
                dir="${repos:t}:${dir:t}"
            else
                # name of directory
                dir=${dir:t}
            fi
            _screen_set_title "$dir"
        fi
    }
}
typeset -A SCREEN_TITLE_CMD_ARG; SCREEN_TITLE_CMD_ARG=(ssh -1 su -1 man -1)
typeset -A SCREEN_TITLE_CMD_IGNORE; SCREEN_TITLE_CMD_IGNORE=()
function { # use command name as a title
    function _screen_set_cmd_title () {
        local -a cmd; cmd=(${(z)1})
        while [[ "$cmd[1]" =~ "[^\\]=" ]]; do shift cmd; done
        [[ "$cmd[1]" == "command" ]] && shift cmd
        [[ "$cmd[1]" == "builtin" ]] && shift cmd
        [[ "$cmd[1]" == "env" ]] && shift cmd
        [[ "$cmd[1]" == "/usr/bin/env" ]] && shift cmd
        if [[ -n "$SCREEN_TITLE_CMD_IGNORE[$cmd[1]]" ]]; then
            return
        elif [[ -n "$SCREEN_TITLE_CMD_ARG[$cmd[1]]" ]]; then
            # argument of command
            cmd[1]=$cmd[$SCREEN_TITLE_CMD_ARG[$cmd[1]]]
        fi
        _screen_set_title "$cmd[1]:t"
    }
    function preexec_screen_window_title () {
        local -a cmd; cmd=(${(z)2}) # command in a single line
        if [[ "$SCREENTITLE" = 'auto' ]]; then
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
function screen_title() {
    if [[ -n "$SCREENTITLE" ]]; then
        if [[ -n "$1" ]]; then
            # set title explicitly
            export SCREENTITLE=explicit
            _screen_set_title "$1"
        else
            # automatically set title
            export SCREENTITLE=auto
        fi
    fi
}

precmd_functions+=precmd_screen_window_title
preexec_functions+=preexec_screen_window_title
