# VCS
autoload -Uz vcs_info
zstyle ':vcs_info:(git|svn):*' formats '%R' '%S' '%b' '%s'
zstyle ':vcs_info:(git|svn):*' actionformats '%R' '%S' '%b|%a' '%s'
zstyle ':vcs_info:*' formats '%R' '%S' '%s:%b' '%s'
zstyle ':vcs_info:*' actionformats '%R' '%S' '%s:%b|%a' '%s'
function precmd_vcs_info () {
    psvar=()
    STY= LANG=en_US.UTF-8 vcs_info
    repos=`print -nD "$vcs_info_msg_0_"`
    if [[ -n "$vcs_info_msg_1_" ]]; then
        vcs="$vcs_info_msg_3_"
    else
        vcs=''
    fi
    [[ -n "$repos" ]] && psvar[2]="$repos"
    [[ -n "$vcs_info_msg_1_" ]] && psvar[3]="$vcs_info_msg_1_"
    [[ -n "$vcs_info_msg_2_" ]] && psvar[1]="$vcs_info_msg_2_"
}

# set window title of screen
function set_screen_title () { echo -ne "\ek$1\e\\" }
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
            set_screen_title "$dir"
        fi
    }
}
typeset -A SCREEN_TITLE_CMD_ARG; SCREEN_TITLE_CMD_ARG=(ssh -1 su -1 man -1)
typeset -A SCREEN_TITLE_CMD_IGNORE; SCREEN_TITLE_CMD_IGNORE=()
function { # use command name as a title
    function set_cmd_screen_title () {
        local -a cmd; cmd=(${(z)1})
        while [[ "$cmd[1]" =~ "[^\\]=" ]]; do shift cmd; done
        if [[ "$cmd[1]" == "env" ]]; then shift cmd; fi
        if [[ -n "$SCREEN_TITLE_CMD_IGNORE[$cmd[1]]" ]]; then
            return
        elif [[ -n "$SCREEN_TITLE_CMD_ARG[$cmd[1]]" ]]; then
            # argument of command
            cmd[1]=$cmd[$SCREEN_TITLE_CMD_ARG[$cmd[1]]]
        fi
        set_screen_title "$cmd[1]:t"
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
                    set_cmd_screen_title "$cmd"
                    return
                    ;;
            esac
            # resolve command in jobs
            local -A jt; jt=(${(kv)jobtexts})
            $cmd >>(read num rest
                cmd=(${(z)${(e):-\$jt$num}})
                set_cmd_screen_title "$cmd"
            ) 2>/dev/null
        fi
    }
}
function title() {
    if [[ -n "$SCREENTITLE" ]]; then
        if [[ -n "$1" ]]; then
            # set title explicitly
            export SCREENTITLE=explicit
            set_screen_title "$1"
        else
            # automatically set title
            export SCREENTITLE=auto
        fi
    fi
}

precmd_functions+=precmd_vcs_info
precmd_functions+=precmd_screen_window_title
preexec_functions+=preexec_screen_window_title

# prompt
PROMPT="%(!.%F{red}.%F{green})%U%n@%6>>%m%>>%u%f:%1(j.%j.)%(!.#.>) "
local psdirs='[%F{yellow}%3(v|%32<..<%3v%<<|%60<..<%~%<<)%f]'
local psvcs='%3(v|[%25<\<<%F{yellow}%2v%f@%F{blue}%1v%f%<<]|)'
RPROMPT="$psdirs$psvcs"
