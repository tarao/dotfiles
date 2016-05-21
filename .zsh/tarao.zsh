[[ -d ~/.fzf ]] && {
    bindkey '^R' fzf-history-widget
    bindkey '^]' fzf-ghq-widget
}

SCREEN_TITLE_CMD_ARG+=(v -1 vi -1 git 2)

EMACS_USE_DAEMON=1
