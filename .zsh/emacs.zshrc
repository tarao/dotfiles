alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacsd='emacs-snapshot --daemon'
alias emacs22='env emacs22'
alias emacs23='env emacs-snapshot'
alias emacs-standalone='emacs23'
function emacsb {
    env emacs-snapshot --batch $@
}
alias emacs-compile="emacsb -f batch-byte-compile"
function emacsbinstall {
    emacsb -l ~/.emacs.d/dot/install.el $@
}
function install-elisp {
    emacsbinstall --eval "(install-elisp \"$1\")"
}
function update-elisp {
    emacsbinstall -f update-remote-emacs-lisp
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
