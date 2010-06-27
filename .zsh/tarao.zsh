SCREEN_TITLE_CMD_ARG=(ssh -1 su -1 man -1 v -1)
bindkey '^R' anything-history
bindkey -M afu >/dev/null 2>&1 && bindkey -M afu '^R' anything-history

unalias emacs
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
