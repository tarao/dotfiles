# suppress suspend by C-s
stty stop undef

# remove duplicated path
typeset -gxU PATH=$PATH

# history
HISTSIZE=100000
SAVEHIST=100000
setopt share_history
setopt hist_ignore_space
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# snatch stdout of existing process
# see http://subtech.g.hatena.ne.jp/cho45/20091118/1258554176
function snatch() {
    gdb -p $1 -batch -n -x \
        =(echo "p (int)open(\"/proc/$$/fd/1\", 1)
                p (int)dup2(\$1, 1)
                p (int)dup2(\$1, 2)")
}

# git-hg compatibility
function git() {
    if [[ "$vcs" = 'hg' ]]; then
        local args
        args=`git2hg $@`
        hg ${=args}
    else
        env git $@
    fi
}

# alias
alias sc='screen -h 4096'
alias wcat='wget -q -O -'

export MANPAGER='less -s'
export PAGER='v'
alias man='LANG=${LANG/en_US.UTF-8/en_US} env man'
alias diff='colordiff -u'
alias od='od -A x -t xCz'

cppwarning="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
alias c++="env g++ -lstdc++ -std=c++98 -pedantic-errors $cppwarning"
alias c++now="env g++ -lstdc++ -std=c++98 -pedantic-errors"
alias g++="env g++ -lstdc++ -std=gnu++98 -pedantic $cppwarning"
alias g++now="env g++ -lstdc++ -std=gnu++98 -pedantic"

alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacsd='emacs-snapshot --daemon'
alias emacs22='env emacs22'
alias emacs23='env emacs-snapshot'
alias emacs-standalone='emacs23'
alias emacs-compile='emacs-standalone -batch -f batch-byte-compile'

# Emacs server and client
function emacs() {
    if [[ `id -ur` = 0 ]]; then # root
        emacs-standalone $@
    else
        if [[ -z `pgrep emacs -u $USER` ]]; then
            emacsd
        fi
        emacsc $@
    fi
}
function stop-emacsd() {
    if [[ -n `pgrep emacs -u $USER` ]]; then
        emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
    fi
}
function restart-emacsd() {
    stop-emacsd
    emacsd
}

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
    precmd_screen_window_title () {
        if [[ "$SCREENTITLE" = 'auto' ]]; then
            local dir=`pwd`
            dir=`print -nD $dir`
            if [[ ( -n "$vcs" ) && ( "$repos" != "$dir" ) ]]; then
                # name of repository and directory
                dir="${repos:t}:${dir:t}"
            else
                # name of directory
                dir=${dir:t}
            fi
            screen -X eval "title '$dir'"
        fi
    }
    typeset -A SCREEN_TITLE_CMD_ARG
    typeset -A SCREEN_TITLE_CMD_IGNORE
    SCREEN_TITLE_CMD_ARG=()
    SCREEN_TITLE_CMD_IGNORE=(fg 1 job 1)
    preexec_screen_window_title () {
        typeset -a ZSH_LAST_CMD
        ZSH_LAST_CMD=(${=1})
        if [[ "$SCREENTITLE" = 'auto' ]]; then
            # name of command
            local j=$ZSH_LAST_CMD[1]
            if [[ -n "$SCREEN_TITLE_CMD_IGNORE[$j]" ]]; then
                j=$SCREEN_TITLE_CMD_LAST
            else
                if [[ -n "$SCREEN_TITLE_CMD_ARG[$j]" ]]; then
                    # argument of command
                    j=$ZSH_LAST_CMD[$SCREEN_TITLE_CMD_ARG[$j]]
                fi
            fi
            SCREEN_TITLE_CMD_LAST=$j
            screen -X eval "title '${j:t}'"
        fi
    }
    function title() {
        if [[ -n "$SCREENTITLE" ]]; then
            if [[ -n "$1" ]]; then
                # set title explicitly
                export SCREENTITLE=explicit
                screen -X eval "title '$1'"
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
