[[ -d ~/.fzf ]] && {
    bindkey '^R' fzf-history-widget
    bindkey '^S' fzf-find-file-widget
    bindkey '^Q' fzf-ghq-widget
}

SCREEN_TITLE_CMD_ARG+=(v -1 vi -1 git 2)

EMACS_USE_DAEMON=1
