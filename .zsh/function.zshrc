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
