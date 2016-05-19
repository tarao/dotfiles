#! /usr/bin/env zsh

source ~/.zsh/screen.zsh
source ~/.zsh/screen-attach.zsh
source ~/.zsh/emacs.zsh
source ~/.zsh/local.zsh

left="${EMACS_LEFT:-nil}"
right="${EMACS_RIGHT:-nil}"
top="${EMACS_TOP:-nil}"
bottom="${EMACS_BOTTOM:-nil}"
width="${EMACS_WIDTH:-80}"
offset_left="${EMACS_OFFSET_LEFT:-0}"
offset_top="${EMACS_OFFSET_TOP:-0}"
height="${EMACS_HEIGHT:-nil}"
[[ -z "$SCREEN_EXPORT_ENV" ]] && SCREEN_EXPORT_ENV=(
    DISPLAY XAUTHORITY
    SSH_CONNECTION SSH_CLIENT SSH_TTY SSH_AUTH_SOCK SSH_AGENT_PID
    GNOME_KEYRING_CONTROL GNOME_KEYRING_PID
    GDM_XSERVER_LOCATION GDMSESSION
    XDG_SESSION_COOKIE
    DESKTOP_SESSION SESSION_MANAGER
    GPG_AGENT_INFO
    GTK_IM_MODULE QT_IM_MODULE
    WINDOWID WINDOWPATH
    DBUS_SESSION_BUS_ADDRESS
)

EMACS_USE_DAEMON=1
function emacsc(){ emacsclient -c "$@" }
function emacsc_msg(){ emacsclient --eval "(message \"$1\")" >/dev/null 2>&1 }

function emacsd_progress_start () { # show progress dialog
    local w=0.3
    local i=0

    # no approximation: just move the progress bar
    function emacsd_progress_next(){ (( i+=10 )) }

    [ -n "$1" ] && [ $1 -gt 0 ] && {
        # approximation by the number of lines in the startup message
        w=0.05
        local len="$1"
        function emacsd_progress_next () {
            i=`wc -l "$EMACS_DAEMON_LOG" | cut -f 1 -d ' '`
            (( i = $i * 100 / $len ))
        }
    }

    function emacsd_progress () {
        while (( i < 100 )); do
            emacsc_msg 'ping' && echo 100 && return # the server is responding
            echo $i
            sleep $w
            emacsd_progress_next
        done
    }

    whence zenity >/dev/null && {
        ( emacsd status >/dev/null || {
                emacsd_progress | zenity --progress --auto-close \
                    --text "Running Emacs daemon..."
        }; emacsd update-env $SCREEN_EXPORT_ENV; emacsc_msg 'ready' ) &
    }
}

N=0
emacsd status >/dev/null || {
    N=`wc -l "$EMACS_DAEMON_LOG" 2>/dev/null | cut -f 1 -d ' '`
    echo > "$EMACS_DAEMON_LOG" # clear
}
emacsd_progress_start "$N"
emacs --eval \
"(let ((left $left) (right $right) (top $top) (bottom $bottom)             \
       (width $width) (height $height)                                     \
       (offset-left $offset_left) (offset-top $offset_top))                \
   (set-frame-position (selected-frame) offset-left offset-top)            \
   (set-frame-width (selected-frame) width)                                \
   (unless height                                                          \
     (setq height (/ (- (x-display-pixel-height) offset-top)               \
                     (frame-char-height))))                                \
   (set-frame-height (selected-frame) height)                              \
   (when (and (not left) right)                                            \
     (setq left (- (x-display-pixel-width) (frame-pixel-width) right)))    \
   (when (and (not top) bottom)                                            \
     (setq top (- (x-display-pixel-height) (frame-pixel-height) bottom)))  \
   (setq left (or left (frame-parameter (selected-frame) 'left))           \
         top (or top (frame-parameter (selected-frame) 'top)))             \
   (set-frame-position (selected-frame) left top))" "$@"
