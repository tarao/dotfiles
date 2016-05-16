bindkey '^]' _ghq_select

SCREEN_TITLE_CMD_ARG+=(v -1 vi -1 git 2)
[[ -n "$eterm_options[(r)term:*+]" ]] || bindkey '^R' anything-history

EMACS_USE_DAEMON=1
