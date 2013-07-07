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
        ~/.gem/ruby/*/bin(N-/) # ruby
        ~/perl5/perlbrew/perls/current/bin(N-/) # perl
        ~/extlib/bin(N-/)                       # perl
        ~/node_modules/.bin(N-/) # js
        {,/usr/local,/usr}/bin(N-/)
        {/usr,/usr/local}/games(N-/)
    )
    path=($user_path $path)
    [ "`id -u`" -eq 0 ] && path=($root_path $path)
    typeset -gxU path
}
_set_path_env

# perl
function _set_perl_env() {
    local arch; arch="$(perl -MConfig -e 'print $Config{archname}')"
    local extlib; extlib="$HOME/extlib/lib/perl5"
    export PERL5LIB="$extlib:$extlib/$arch"
    whence cpanm >/dev/null && export PERL_CPANM_OPT="--local-lib=~/extlib"
}
whence perl >/dev/null && [[ -d ~/extlib ]] && _set_perl_env
