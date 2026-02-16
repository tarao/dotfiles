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

asdf_sh="$HOME/.asdf/asdf.sh"
[ -r "$asdf_sh" ] && {
    . "$asdf_sh"
}

sdkman_init_sh="$HOME/.sdkman/bin/sdkman-init.sh"
[ -r "$sdkman_init_sh" ] && {
    . "$sdkman_init_sh"
}

# PATH
function _set_path_env() {
    local su_path; su_path=(
        {,/usr/local,/usr}/sbin(N-/)
    )
    path=(
        ~/bin
        ~/bin/tools
        ~/.local/bin
        /snap/bin
        /usr/local/go/bin(N-/) # golang
        "$GOPATH/bin"          # golang
        ~/.cargo/bin           # rust
        ~/.volta/bin           # node
        ~/.anyenv/bin
        $path
    )
    whence anyenv >/dev/null && eval "$(anyenv init -)"
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
