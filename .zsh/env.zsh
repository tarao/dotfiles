ZDOTDIR=~/.zsh

umask 022
unsetopt nomatch

whence xsel >/dev/null && {
    function xsel() {
        [[ -n "$DISPLAY" ]] && command xsel "$@" 2>/dev/null
    }
}

# PATH
function _set_path_env() {
    local su_path; su_path=(
        {,/usr/local,/usr}/sbin(N-/)
    )
    local user_path; user_path=(
        ~/bin
        ~/bin/tools
        $(whence plenv>/dev/null && plenv root)/shims(N-/) # perl
        ~/.gem/ruby/*/bin(N-/) # ruby
        ~/.local/node-current/bin(N-/) # js
        ~/go/bin # golang
        {,/usr/local,/usr}/bin(N-/)
        {/usr,/usr/local}/games(N-/)
    )
    path=($user_path $path)
    [ "`id -u`" -eq 0 ] && path=($su_path $path)
    typeset -gxU path
}
_set_path_env

# perl
function _set_perl_env() {
    whence plenv >/dev/null && {
        eval "$(plenv init -)"
        local global_version=$(plenv global)
        [[ -n "$global_version" ]] || return
        PERL5LIB=$(PERL_VERSION="$global_version" perl -e'print join ";",@INC')
        export PERL5LIB
    }
}
_set_perl_env
