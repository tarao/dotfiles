# snatch stdout of existing process
# see http://subtech.g.hatena.ne.jp/cho45/20091118/1258554176
function snatch() {
    gdb -p $1 -batch -n -x \
        =(echo "p (int)open(\"/proc/$$/fd/1\", 1)
                p (int)dup2(\$1, 1)
                p (int)dup2(\$1, 2)")
}

function watchdir () {
    if [[ "$1" != "" ]]; then
        local dir="$1"
        shift
        if [[ -x "`which inotifywait`" ]]; then
            ls $dir
            while true; do
                inotifywait -q $@ $dir
            done
        else
            echo 'inotifywait not found'
        fi
    else
        echo "Usage: $0 <dir> [-e event1 -e event2 ...]"
    fi
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

function git-set-remote () {
    if [[ "$1" == '-h'  || "$1" == '--help' || "$1" == 'help' ]]; then
        echo "Usage: $0 branch remote"
        return
    fi
    local branch=$1
    local remote=$2
    [[ -z "$brach" ]] &&  branch=master
    [[ -z "$remote" ]] && remote=origin
    git config --add branch.$branch.remote $remote
    git config --add branch.$branch.merge refs/heads/$brach
}
