# require term+
[[ `echo $INSIDE_EMACS | cut -f 2 -d ,` = term:*+ ]] || return

# send OSC 51 ; Ps ; Pd ST commans
function osc_emacs () {
    sleep 0; echo -ne "\e]51;$1;$2\e\\" > /dev/tty
}
# send OSC 52 ; Pc ; Pd ST commans
function osc_sel () {
    sleep 0; echo -ne "\e]52;$1;$2\e\\" > /dev/tty
}

# set current directory
function precmd_eterm_cwd () {
    local dir; dir=`pwd`
    osc_emacs 'cd' "$dir"
}
precmd_functions+=precmd_eterm_cwd

# set host and user
function eterm_current_host () {
    local host; host=`hostname`
    osc_emacs 'host' "$host"
}
function eterm_current_user () {
    local user; user=`id -run`
    osc_emacs 'user' "$user"
}
function eterm_current_histfile () {
    osc_emacs 'histfile' "$HISTFILE"
}

eterm_current_host
eterm_current_user
eterm_current_histfile

# switch to term-line-mode
function switch-to-line-mode () {
    local rprompt="$RPROMPT"
    RPROMPT=''
    local buf="$BUFFER"
    zle kill-buffer
    zle reset-prompt
    zle -R
    osc_emacs 'mode' 'sh-mode' # TODO: check 'mode' flag in $INSIDE_EMACS
    osc_sel "$1" "$buf"
    RPROMPT="$rprompt"
}
function switch-to-line-mode-normal () {
    switch-to-line-mode 'n'
}
function switch-to-line-mode-insert () {
    switch-to-line-mode 'i'
}
function history-search-eterm () {
    local buf="$BUFFER"
    zle kill-buffer
    osc_sel 'h' "$buf"
}

# TODO: check 'evil' flag in $INSIDE_EMACS
zle -N switch-to-line-mode-normal
zle -N switch-to-line-mode-insert
zle -N history-search-eterm

bindkey '^[' switch-to-line-mode-normal
bindkey '^[i' switch-to-line-mode-insert
bindkey '^R' history-search-eterm

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
        osc_emacs 'open' "${(j:;:)inputs}"
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
        osc_emacs 'view' "${(j:;:)inputs}"
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

### cdd

function _define_cdd_alias () {
    if [[ -n "$functions[cdd]" ]]; then
        local code; code="$functions[cdd]"
        eval "function _cdd_original () { $code }"
    elif whence -p cdd >/dev/null; then
        eval 'function _cdd_original () { command cdd "$@" }'
    else
        eval 'function _cdd_original () { }'
    fi
}
_define_cdd_alias

function _cdd_dir () {
    local dir; dir="${1##*:}"
    [[ "$dir" = "$1" ]] && { dir='' }
    echo "$dir"
}

function _cdd_pat () {
    echo "${1%:*}"
}

function _cdd_cd () {
    local dir; dir=`_cdd_dir "$*"`
    echo "$dir"
    cd "$dir"
}

function _cdd_read () {
    local c result cr lf
    cr=`echo -n "\r"`
    lf=`echo -n "\nx"`
    while [[ "$c" != "\n" ]]; do
        result="$result$c"
        read -k -r -s c 2>/dev/null
        [[ "$c" == "\r" || "$c"x == "$lf" || "$c" == "$cr" ]] && { c="\n" }
    done
    echo "$result"
}

function _cdd_matches () {
    local prefix
    [[ "$1" = '-p' ]] && { prefix='yes'; shift }

    local pat reply; pat=`_cdd_pat "$*"`
    osc_emacs 'cdd' ":$pat"
    reply=`_cdd_read`
    local -Ua matches; matches=(${(ps:\t:)reply})

    [[ -n "$prefix" ]] && {
        matches=($matches ${(@)matches#<0-9>*:}) # add entries without index
        matches=(${(M)matches:#${pat}*})
    }

    echo ${(F)matches}
}

function _cdd_bookmarks () {
    local prefix
    [[ "$1" = '-p' ]] && { prefix='yes'; shift }

    local -a bookmarks; bookmarks=(`_cdd_original`)
    bookmarks=(${bookmarks:#<0-9>*})

    local pat; pat=`_cdd_pat "$*"`
    bookmarks=(${(M)bookmarks:#${pat}*})

    if [[ -n "$prefix" ]]; then
        bookmarks=(${(M)bookmarks:#${pat}*})
    else
        bookmarks=(${(M)bookmarks:#*${pat}*:*})
    fi

    echo ${(F)bookmarks}
}

function _cdd_select () {
    local dir; dir=`_cdd_dir "$*"`
    [[ -n "$dir" ]] && { _cdd_cd "$*"; return }

    local -a matches; matches=(`_cdd_matches "$@"`)
    matches=($matches `_cdd_bookmarks "$@"`)

    if [[ "$#matches" = 1 ]]; then
        _cdd_cd "$matches[1]"
    else
        echo ${(F)matches}
    fi
}

function cdd () {
    [[ "$1" = '-h' || "$1" = '--help' ]] && {
        echo "Usage: $0"
        echo "       $0 -l"
        echo "       $0 [-m] [PATTERN...][:DIRECTORY]"
        echo "       $0 -b [NAME]"
        [[ -n "$functions[_cdadd]" ]] && echo "       $0 -a NAME DIRECTORY"
        [[ -n "$functions[_cddel]" ]] && echo "       $0 -d NAME"
        echo "Options:"
        echo "  <no arguments>  Select tab interactively"
        echo "  -l              List all entries"
        echo "  -m              PATTERN can match with the middle of text"
        echo "  PATTERN...      Patterns to be matched"
        echo "  :DIRECTORY      If this is specified, move to that directory"
        echo "  -b              Look up bookmarks"
        [[ -n "$functions[_cdadd]" ]] && {
            echo "  -a              Add an entry of a DIRECTORY with a NAME to the bookmarks"
        }
        [[ -n "$functions[_cddel]" ]] && {
            echo "  -d              Delete the entry of a directory named NAME from the bookmarks"
        }
        return
    }

    if [[ -z "$1" ]]; then # TODO: check 'mux' flag in $INSIDE_EMACS
        osc_emacs 'cdd' "$*"
        read -r -s
        _cdd_cd "$REPLY"
    elif [[ "$1" = '-a' && -n "$functions[_cdadd]" ]]; then
        shift; _cdadd "$@"
    elif [[ "$1" = '-d' && -n "$functions[_cddel]" ]]; then
        shift; _cddel "$@"
    elif [[ "$1" = '-b' ]]; then  # TODO: check 'mux' flag in $INSIDE_EMACS
        shift; _cdd_original "$@"
    elif [[ "$1" = '-m' || "$1" = '-l' ]]; then
        shift; _cdd_select "$@"
    else
        _cdd_select -p "$@"
    fi
}

function _cdd_describe_matches () {
    local arg;
    [[ -n "$line" && "$+opt_args[-m]" != 1 ]] && { arg='-p' }
    matches=(`_cdd_matches $arg`)
    matches=(${(@)matches//:/\\:})
    _describe -t directories "directory in tab" matches
}

function _cdd_describe_bookmarks () {
    bookmarks=(`_cdd_bookmarks`)
    [[ "$state" = bdel ]] && { bookmarks=(${(@)bookmarks%%:*}) }
    bookmarks=(${(@)bookmarks//:/\\:})
    _describe -t bookmarks "bookmark" bookmarks
}

function _cdd () {
    typeset -A opt_args
    local context state line
    local -a bookmarks
    local -a matches

    _arguments -s \
        "(-m -b -a -d)-l[list all entries]" \
        "(-l -b -a -d)-m[match with middle of text]" \
        "(-m -l -a -d)-b[look up bookmarks]:bookmark name:->blookup" \
        "(-m -l -b -d)-a+[add bookmark]:bookmark name: :directory:_files -/" \
        "(-m -l -b -a)-d[delete bookmark]:bookmark name:->bdel" \
        "(-l -b -a -d):pattern:->pattern" \
        && return 0

    case $state in
    blookup|bdel)
        _cdd_describe_bookmarks
        [[ "$#bookmarks" != 0 ]] && return 0
        ;;
    pattern)
        _cdd_describe_matches
        _cdd_describe_bookmarks
        [[ "$#matches" != 0 || "$#bookmarks" != 0 ]] && return 0
        ;;
    esac

    return 1
}

compdef _cdd cdd
