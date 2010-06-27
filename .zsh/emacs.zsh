alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacs-standalone='emacs-snapshot'
alias emacs='emacs-standalone'
function emacsb {
    [[ -z "$1" ]] &&
    echo "Usage: $0 [compile FILE | install URL | update]..." && return
    cmd=(env emacs-snapshot --batch)
    install=($cmd -l ~/.emacs.d/dot/install.el)
    action=$1; shift
    case "$action" in
        compile)
            $cmd -f batch-byte-compile $@
            ;;
        install)
            url=$1; shift
            $install --eval "(install-elisp \"$url\")" $@
            ;;
        update)
            $install -f update-remote-emacs-lisp $@
            ;;
        help)
            $0
            ;;
        *)
            $cmd $@
            ;;
    esac
}
alias emacs-compile="emacsb compile"

# Emacs server
function emacsd() {
    [[ -z "$1" ]] && emacs-snapshot --daemon && return
    action=$1; shift
    case "$action" in
        start)
            $0
            ;;
        stop)
            [[ -n `pgrep emacs -u $USER` ]] &&
            emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
            ;;
        restart)
            $0 stop
            $0 start
            ;;
        *)
            echo "Usage: $0 [start|stop|restart]"
            ;;
    esac
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
