# snatch stdout of existing process
# see http://subtech.g.hatena.ne.jp/cho45/20091118/1258554176
function snatch() {
    gdb -p $1 -batch -n -x \
        =(echo "p (int)open(\"/proc/$$/fd/1\", 1)
                p (int)dup2(\$1, 1)
                p (int)dup2(\$1, 2)")
}

# Go up
# see http://subtech.g.hatena.ne.jp/secondlife/20080604/1212562182
# and http://d.hatena.ne.jp/hitode909/20100211/1265879271
function _gu () {
    [[ -z "$1" || "${1[1]}" = '/' ]] && {
        [[ -n "$vcs" ]] || return
        # move to repository root
        if [[ "$vcs" = 'git' ]]; then
            local root; root=`git rev-parse --show-cdup`
            [[ -n "$root" ]] && cd "${root:a}"
        else
            $0 ".$vcs"
        fi
        [[ -n "$1" ]] && cd "${1[2,-1]}"
        return
    }
    local parent; parent='.'
    while [[ "${parent:a}" != "/" ]]; do
        parent="../$parent"
        [[ "${parent:a:t}" = "$1" ]] && cd "$parent" && return
        [[ -e "$parent/$1" ]] && cd "$parent" && return
    done
    return 1
}
function gu () {
    [[ "$1" = '-h' || "$1" = '--help' ]] && {
        echo "Usage: $0 [ /<relative> | <directory> | <file> ]"
        cat <<EOT
Go up to the repository root.
Options:
  /<relative>    The destination is <relative> under the repository root.
  <directory>    The destination is an ancestor of the current directory,
                 where the name of the ancestor matches <directory>.
  <file>         The destination is an ancestor of the current directory,
                 where the ancestor contains a file whose name matches <file>.
EOT
        return
    }
    local stack; stack=("${${(f)$(dirs -lp)}[@]}")
    local dir; dir="${$(pwd):a}"
    _gu $@
    dirs $stack
    [[ "$dir" != "${$(pwd):a}" ]] && return
    popd
    [[ -z "$1" ]] && cd ..
}

function watchdir () {
    [[ -z "$1" ]] && {
        echo "Usage: $0 <dir> [-e event1 -e event2 ...]"
        return
    }

    whence inotifywait >/dev/null || {
        echo "$0: inotifywait not found" > /dev/stderr
        return
    }

    local dir="$1"; shift
    ls $dir
    while true; do
        inotifywait -q "$@" "$dir"
    done
}

# git-hg compatibility
function git() {
    if [[ "$vcs" = 'hg' ]]; then
        local args; args=`git2hg $@`
        hg ${=args}
    elif [[ "$1" = 'merge' ]]; then
        shift
        if [[ "$1" = -* ]]; then
            command git merge $@
        else
            command git merge --no-ff $@
        fi
    else
        command git $@
    fi
}

function git-set-remote () {
    if [[ "$1" == '-h'  || "$1" == '--help' || "$1" == 'help' ]]; then
        echo "Usage: $0 <remote=origin> <branch=CURRENT_BRANCH>"
        return
    fi
    local remote=$1
    local branch=$2
    [[ -n "$remote" ]] || remote=origin
    [[ -n "$branch" ]] || branch=`git rev-parse --abbrev-ref HEAD`
    git config --add "branch.$branch.remote" "$remote"
    git config --add "branch.$branch.merge" "refs/heads/$branch"
}

# file conversion
function pdf2svg {
    [[ "$1" == '-h' || "$1" == '--help' ]] && {
        echo "Usage: $0 [-p <page>] <input> <output>"
        return
    }

    local page; page=1
    [[ "$1" == '-p' ]] && {
        shift; page="$1"; shift
    }
    local catpdf; catpdf=(pdftk "$1" cat $page output -)
    pstoedit -f plot-svg -dt -ssp =($catpdf) "$2"
}
