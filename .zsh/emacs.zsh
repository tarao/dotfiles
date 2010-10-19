alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacs-standalone='emacs-snapshot'
function emacsb {
    [[ -z "$1" ]] &&
    echo "Usage: $0 [compile FILE | install URL | update]..." && return
    local cmd; cmd=`alias -m emacs-standalone | cut -f2 -d=`
    cmd=($cmd --batch)
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
    [[ -z "$1" ]] && 1='help'
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
            if $0 status >/dev/null; then
                echo 'emacs daemon is already running'
                return 1
            fi
            $cmd
            ;;
        stop)
            $0 status >/dev/null &&
            emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
            ;;
        restart)
            $0 stop
            local -i c; c=0
            while (( c < 10 )) && $0 status >/dev/null; do
                (( c++ ))
                sleep 0.1
            done
            $0 start
            ;;
        *)
            echo "Usage: $0 status|start|stop|restart"
            ;;
    esac
}

function _emacs_get_comm () {
    local -a opts
    [[ -n "$EMACS_SERVER_FILE" ]] && opts[1]="$EMACS_SERVER_FILE"
    zparseopts -E -a opts s: -socket-name: f: -server-file:
    (( $#opts > 0 )) && echo ${opts[-1]#=}
}

function emacs() {
    if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
        emacs-standalone $@
    else
        emacsd status >/dev/null || emacsd start
        [[ -n "$STY" ]] && {
            # identifier of the target emacs daemon
            local comm; comm=`_emacs_get_comm $@`
            [[ -z "$comm" ]] && comm='default'

            # get daemons already registered
            local reg; reg=`screen_getenv "$STY" SCREEN_EMACSD`;
            reg=${(s.:.)reg}

            # register emacs daemon to screen
            [[ -z "$reg[(r)$comm]" ]] && {
                reg=($reg $comm)
                screen_setenv "$STY" SCREEN_EMACSD "${(j.:.)reg}"
                local num; num=$#reg; local hook;
                hook="emacsclient $@ -e '(screen-sync-env \"$STY\")'"
                screen_add_attach_hook "$STY" "SCREEN_EMACSD_ENV$num" "$hook"
            }
        }
        DISPLAY="$DISPLAY" emacsc $@
    fi
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
