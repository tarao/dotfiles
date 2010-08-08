alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacs-standalone='emacs-snapshot'
alias emacs='emacs-standalone'
function emacsb {
    [[ -z "$1" ]] &&
    echo "Usage: $0 [compile FILE | install URL | update]..." && return
    local cmd; cmd=(env emacs-snapshot --batch)
    local install; install=($cmd -l ~/.emacs.d/dot/install.el)
    local action; action=$1; shift
    case "$action" in
        compile)
            $cmd -f batch-byte-compile $@
            ;;
        install)
            local url; url=$1; shift
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
    local cmd; cmd=`alias -m emacs-standalone | cut -f2 -d=`
    cmd=($cmd --daemon)
    [[ -z "$1" ]] && $cmd && return
    local action; action=$1; shift
    case "$action" in
        status)
            local grep; grep=(pgrep -f -u $USER "$cmd")
            if [[ -n `$grep` ]]; then
                echo 'emacs daemon is running'
                return 0
            fi
            echo 'emacs daemon is not running'
            return 1
            ;;
        start)
            $0
            ;;
        stop)
            $0 status >/dev/null &&
            emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
            ;;
        restart)
            $0 stop
            $0 start
            ;;
        *)
            echo "Usage: $0 [status|start|stop|restart]"
            ;;
    esac
}

# See: http://d.hatena.ne.jp/rubikitch/20091208/anythingzsh
function anything-history() {
    local tmpfile; tmpfile=`mktemp`
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
