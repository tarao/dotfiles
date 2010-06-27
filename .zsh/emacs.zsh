alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacsd='emacs-snapshot --daemon'
alias emacs-standalone='emacs-snapshot'
alias emacs='emacs-standalone'
function emacsb {
    env emacs-snapshot --batch $@
}
alias emacs-compile="emacsb -f batch-byte-compile"
function emacsbinstall {
    emacsb -l ~/.emacs.d/dot/install.el $@
}
function install-elisp {
    local install=$1
    shift
    emacsbinstall --eval "(install-elisp \"$install\")" $@
}
function update-elisp {
    emacsbinstall -f update-remote-emacs-lisp $@
}

# Emacs server and client
function stop-emacsd() {
    if [[ -n `pgrep emacs -u $USER` ]]; then
        emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))' $@
    fi
}
function restart-emacsd() {
    stop-emacsd $@
    emacsd $@
}

# See: http://d.hatena.ne.jp/rubikitch/20091208/anythingzsh
function anything-history() {
    local tmpfile
    tmpfile=`mktemp`
    emacsclient -nw --eval \
        "(anything-zsh-history-from-zle \"$tmpfile\" \"$BUFFER\")"
    if [[ -n "$STY" ]]; then
        # screen 4.0.3 has a bug that altscreen doesn't work for emacs
        (( `screen -v | cut -f 3 -d ' ' | cut -f 2 -d.` < 1 )) && zle -I
    fi
    zle -R -c
    if [[ -n "$ANYTHING_HISTORY_DONT_EXEC" ]]; then
        zle -U "`cat $tmpfile`"
    else
        BUFFER="`cat $tmpfile`"
        [[ -n "$BUFFER" ]] && zle accept-line
    fi
    rm $tmpfile
}
zle -N anything-history
