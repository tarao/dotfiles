[[ -d ~/.fzf ]] && {
    export PATH="$PATH:$HOME/.fzf/bin"
    export MANPATH="$MANPAH:$HOME/.fzf/man"

    export FZF_DEFAULT_OPTS="--reverse --inline-info"

    function fzf-history-widget () {
        local selected num
        selected=($(fc -lid 1 | fzf +s --tac +m --with-nth=2.. --tiebreak=index --toggle-sort=ctrl-r ${=FZF_CTRL_R_OPTS} -q "${LBUFFER//$/\\$}"))
        if [ -n "$selected" ]; then
            num=$selected[1]
            if [ -n "$num" ]; then
                zle vi-fetch-history -n $num
            fi
        fi
        zle redisplay
    }
    zle -N fzf-history-widget

    function _fzf_ghq_filter () {
        local -a roots
        roots=($(git config --get-all ghq.root))
        (( $#roots < 1 )) && roots[1]='~/.ghq'

        local repository r components
        while read repository; do
            repository="$(print -nD "$repository")"
            for r in $roots; do
                r="${r%/}/"
                components=(${(s:/:)repository#$r})
                [[ "$repository" = "$r"* ]] && {
                    echo "$r ${repository#$r} ${(j:[34m/[m:)components[2,-1]}	[36m$components[1][m	[1;30m[${r%/}][m"
                    break
                }
            done
        done
    }

    function fzf-ghq-widget () {
        local -a repository=($(ghq list -p 2>/dev/null | _fzf_ghq_filter | fzf +s --ansi --with-nth=3..))
        zle -I
        zle -R -c
        [[ -n "$repository" ]] && {
            BUFFER="cd ${(j::)repository[1,2]}"
            zle accept-line
        }
    }
    zle -N fzf-ghq-widget
}
