if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    # VCS
    autoload -Uz vcs_info
    zstyle ':vcs_info:(git|svn):*' formats '%R' '%S' '%b' '%s'
    zstyle ':vcs_info:(git|svn):*' actionformats '%R' '%S' '%b|%a' '%s'
    zstyle ':vcs_info:*' formats '%R' '%S' '%s:%b' '%s'
    zstyle ':vcs_info:*' actionformats '%R' '%S' '%s:%b|%a' '%s'
    precmd_vcs_info () {
        psvar=()
        LANG=en_US.UTF-8 vcs_info
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
    function set_screen_title () { echo -n "k$1\\" }
    function { # use current directory as a title
        precmd_screen_window_title () {
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
    function { # use command name as a title
        typeset -A SCREEN_TITLE_CMD_ARG; SCREEN_TITLE_CMD_ARG=()
        typeset -A SCREEN_TITLE_CMD_IGNORE; SCREEN_TITLE_CMD_IGNORE=()
        function set_cmd_screen_title () {
            local -a cmd; cmd=(${(z)1})
            while [[ "$cmd[1]" =~ "[^\\]=" ]]; do shift cmd; done
            if [[ "$cmd[1]" == "env" ]]; then shift cmd; fi
            if [[ -n "$SCREEN_TITLE_CMD_IGNORE[$cmd[1]]" ]]; then
                return
            else
                if [[ -n "$SCREEN_TITLE_CMD_ARG[$cmd[1]]" ]]; then
                    # argument of command
                    cmd[1]=$cmd[$SCREEN_TITLE_CMD_ARG[$cmd[1]]]
                fi
            fi
            set_screen_title "$cmd[1]:t"
        }
        preexec_screen_window_title () {
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

    typeset -ga precmd_functions
    precmd_functions+=precmd_vcs_info
    precmd_functions+=precmd_screen_window_title

    typeset -ga preexec_functions
    preexec_functions+=preexec_screen_window_title

    # prompt
    PROMPT="%(!.%F{red}.%F{green})%U%n@%6>>%m%>>%u%f:%1(j.%j.)%(!.#.>) "
    local psdirs='[%F{yellow}%3(v|%32<..<%3v%<<|%60<..<%~%<<)%f]'
    local psvcs='%3(v|[%25<\<<%F{yellow}%2v%f@%F{blue}%1v%f%<<]|)'
    RPROMPT="$psdirs$psvcs"
else
    # 0   to restore default color
    # 1   for brighter colors
    # 4   for underlined text
    # 5   for flashing text
    # 30  for black foreground
    # 31  for red foreground
    # 32  for green foreground
    # 33  for yellow (or brown) foreground
    # 34  for blue foreground
    # 35  for purple foreground
    # 36  for cyan foreground
    # 37  for white (or gray) foreground
    # 40  for black background
    # 41  for red background
    # 42  for green background
    # 43  for yellow (or brown) background
    # 44  for blue background
    # 45  for purple background
    # 46  for cyan background
    # 47  for white (or gray) background
    col1='0;4;32'
    col2='0;33'
    PROMPT="%{[${col1}m%}%n@%m%{[m%}:%1(j.%j.)%(!.#.>) "
    RPROMPT="[%{[${col2}m%}%~%{[m%}]"
fi
