ZDOTDIR=~/.zsh
unsetopt nomatch
export PATH=$HOME/bin:$PATH

function xsel() {
    [[ -n "$DISPLAY" ]] && env xsel $@
}
