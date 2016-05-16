WORDCHARS=${WORDCHARS:s,/,,}

function _ghq_select () {
    local repository=$(ghq list -p | peco)
    zle -I
    zle -R -c
    [[ -n "$repository" ]] && {
        BUFFER="cd $repository"
        zle accept-line
    }
}
zle -N _ghq_select
