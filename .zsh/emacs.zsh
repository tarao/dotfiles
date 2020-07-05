EMACS_CLIENT_CMD=(command emacsclient-snapshot)
EMACS_CLIENT_APP='emacs-client'
EMACS_STANDALONE_CMD=(command emacs-snapshot)
EMACS_DAEMON_LOG="$HOME/.emacs.d/daemon.log"
function emacsclient () {
    XMODIFIERS='@im=none' $EMACS_CLIENT_CMD "$@"
}
function emacsc () {
    emacsclient -nw "$@"
}
function emacsclient_desktop () {
    gtk-launch "$EMACS_CLIENT_APP" >/dev/null 2>&1
}
function emacs-standalone () {
    XMODIFIERS='@im=none' $EMACS_STANDALONE_CMD "$@"
}

function emacsb () {
    [[ -z "$1" ]] && {
        cat <<EOF
Usage: $0 [compile [-p] [-L dir ...] FILE]...
EOF
        return
    }
    local cmd; cmd=(emacs-standalone --batch)
    local -a libs; libs=()
    local compile; compile=($cmd -l ~/.emacs.d/init/compile.el)
    local action; action=$1; shift
    local package=0
    [ "x$1" = 'x-p' ] && { package=1; shift; }
    while [[ "x$1" = 'x-L' ]]; do
        libs=($libs $1 "$2"); shift; shift
    done
    case "$action" in
    compile)
        if [ $package = 1 ]; then
            $compile -L . $libs -f batch-byte-compile-with-package "$@"
        else
            $cmd -L . $libs -f batch-byte-compile "$@"
        fi
        ;;
    help)
        $0
        ;;
    *)
        $cmd "$@"
        ;;
    esac
}
alias emacs-compile="emacsb compile"

# Emacs server
function emacsd () {
    local cmd; cmd=(emacs-standalone --daemon)
    [[ -z "$1" ]] && 1='help'
    local action; action=$1; shift
    case "$action" in
    status)
        cmd=($EMACS_STANDALONE_CMD)
        [[ "$cmd[1]" == 'command' ]] && cmd=$cmd[2,-1]
        local grep; grep=(pgrep -f -u $USER "^$cmd --daemon")
        if [[ -n `$grep` ]]; then
            echo 'emacs daemon is running'
            return 0
        fi
        echo 'emacs daemon is not running'
        return 1
        ;;
    echo)
        cmd=($EMACS_CLIENT_CMD)
        $0 status >/dev/null &&
            $cmd --eval "(message \"$1\")" >/dev/null 2>&1 && return 0
        return 1
        ;;
    start)
        $0 status >/dev/null && {
            echo 'emacs daemon is already running'
            return 1
        }
        if [[ -n "$EMACS_DAEMON_LOG" ]]; then
            local log; log="$EMACS_DAEMON_LOG"
            DBUS_SESSION_BUS_ADDRESS= SESSION_MANAGER= $cmd 2>&1 | tee "$log"
        else
            DBUS_SESSION_BUS_ADDRESS= SESSION_MANAGER= $cmd
        fi
        ;;
    stop)
        cmd=($EMACS_CLIENT_CMD)
        $0 status >/dev/null &&
            $cmd -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
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
    setenv)
        cmd=($EMACS_CLIENT_CMD)
        local bs='\\\\'
        local val="$2"; val="${val//\\/${~bs}}"; val="${val//\"/\\\"}"
        $cmd -e "(let ((val \"$val\")) \
                   (setenv \"$1\" (if (> (length val) 0) val nil)))"
        ;;
    update-env)
        while [[ -n "$1" ]]; do
            $0 setenv "$1" "${(P)1}" >/dev/null; shift
        done
        ;;
    wait)
        local w=0.3
        local trial=200
        local i=0
        for (( i=0; $i < $trial; i++ )); do
            $0 echo 'ping' && return 0
            sleep $w
        done
        return 1 # timedout
        ;;
    *)
        echo "Usage: $0 status|start|stop|restart|setenv|update-env"
        ;;
    esac
}

function _emacs_get_comm () {
    local -a opts
    [[ -n "$EMACS_SERVER_FILE" ]] && opts[1]="$EMACS_SERVER_FILE"
    zparseopts -E -a opts s: -socket-name: f: -server-file:
    (( $#opts > 0 )) && echo ${opts[-1]#=}
}

function emacs () {
    if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
        emacs-standalone "$@"
    else
        emacsd status >/dev/null || emacsd start
        [[ -n "$STY" ]] && {
            # identifier of the target emacs daemon
            local comm; comm=`_emacs_get_comm $@`
            [[ -z "$comm" ]] && comm='default'

            # get daemons already registered
            local reg; reg=`screen_getenv "$STY" SCREEN_EMACSD`;
            reg=(${(s.:.)reg})

            # register emacs daemon to screen
            local num; num=$reg[(i)$comm]; local hook
            reg[$num]="$comm"
            screen_setenv "$STY" SCREEN_EMACSD "${(j.:.)reg}"
            hook="emacsclient $@ -e '(screen-sync-env \"$STY\")'"
            screen_add_attach_hook "$STY" "SCREEN_EMACSD_ENV$num" "$hook"
        }
        DISPLAY="$DISPLAY" emacsc "$@"
    fi
}

function emacs-edit () {
    if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
        if [[ "$1" = '-n' ]]; then
            shift
            emacs-standalone "$@" &
        else
            emacs-standalone "$@"
        fi
    else
        emacsd status >/dev/null || {
            emacsclient_desktop
            emacsd wait
        }
        local frames=$(emacsclient -e '(length (visible-frame-list))')
        (( $frames > 1 )) || emacsclient_desktop
        emacsclient "$@" </dev/null >/dev/null
    fi
}
