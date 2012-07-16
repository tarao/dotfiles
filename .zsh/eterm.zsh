function switch-to-line-mode () {
    local rprompt="$RPROMPT"
    RPROMPT=''
    local buf="$BUFFER"
    zle kill-buffer
    zle reset-prompt
    zle -R
    echo -ne "\e]52;$1;$buf\e\\"
    RPROMPT="$rprompt"
}
function switch-to-line-mode-normal () {
    switch-to-line-mode 'n'
}
function switch-to-line-mode-insert () {
    switch-to-line-mode 'i'
}

zle -N switch-to-line-mode-normal
zle -N switch-to-line-mode-insert

bindkey '^[' switch-to-line-mode-normal
bindkey '^[i' switch-to-line-mode-insert
