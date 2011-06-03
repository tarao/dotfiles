SCREEN_TITLE_CMD_ARG+=(v -1 vi -1 git 2)
bindkey '^R' anything-history
bindkey -M afu >/dev/null 2>&1 && bindkey -M afu '^R' anything-history

EMACS_USE_DAEMON=1
