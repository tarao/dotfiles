source ~/.zsh/CVE-2021-45444-VCS_Info-workaround.zsh
source ~/.zsh/tarao.zsh
source ~/.zsh/emacs.tarao.kafka.local.zsh

[ -f ~/.rye/env ] && {
    source ~/.rye/env
}

[ -f ~/.local/share/google-cloud-sdk/path.zsh.inc ] && {
    . ~/.local/share/google-cloud-sdk/path.zsh.inc
}
[ -f ~/.local/share/google-cloud-sdk/completion.zsh.inc ] && {
    . ~/.local/share/google-cloud-sdk/completion.zsh.inc
}
