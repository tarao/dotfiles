SCREEN_TITLE_CMD_ARG=(ssh -1 su -1 man -1 v -1)
bindkey '^R' anything-history
bindkey -M afu >/dev/null 2>&1 && bindkey -M afu '^R' anything-history

EMACS_USE_DAEMON=1
