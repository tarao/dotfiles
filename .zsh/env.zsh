ZDOTDIR=~/.zsh

umask 022
unsetopt nomatch

whence xsel >/dev/null && {
    function xsel() {
        [[ -n "$DISPLAY" ]] && command xsel "$@" 2>/dev/null
    }
}

export GOPATH="$HOME/.go"
whence javac >/dev/null && {
    export JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac))))
}

# PATH
function _set_path_env() {
    local su_path; su_path=(
        {,/usr/local,/usr}/sbin(N-/)
    )
    path=(
        ~/.local/bin
        ~/.anyenv/bin
        /usr/local/go/bin(N-/) # golang
        "$GOPATH/bin"          # golang
        ~/.cargo/bin           # rust
        {,/usr/local,/usr}/bin(N-/)
        {/usr,/usr/local}/games(N-/)
        $path
    )
    [ -r "$HOME/.sdkman/bin/sdkman-init.sh" ] && {
        . "$HOME/.sdkman/bin/sdkman-init.sh"
    }
    whence anyenv >/dev/null && eval "$(anyenv init -)"
    [ -r "$HOME/.asdf/asdf.sh" ] && {
        . "$HOME/.asdf/asdf.sh"
    }
    path=(
        ~/bin
        ~/bin/tools
        $path
    )
    [ "`id -u`" -eq 0 ] && path=($su_path $path)
    typeset -gxU path
}
_set_path_env

# perl
function _set_perl_env() {
    whence plenv >/dev/null && {
        local global_version=$(plenv global)
        [[ -n "$global_version" ]] || return
        PERL5LIB=$(PLENV_VERSION="$global_version" perl -e'print join ";",@INC')
        export PERL5LIB
    }
}
_set_perl_env

# python
function _set_python_env() {
    test -d ~/.poetry/bin/ && {
        path=(
            ~/.poetry/bin
            $path
        )
        typeset -gxU path
    }
}
_set_python_env

function venv_activate() {
    for dir in venv .venv; do
        test -d "$dir" && test -f "$dir"/bin/activate && {
            . "$dir"/bin/activate
            break
        }
    done
}
autoload -Uz add-zsh-hook
add-zsh-hook chpwd venv_activate
