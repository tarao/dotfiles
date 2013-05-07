ZDOTDIR=~/.zsh

if [ "`id -u`" -eq 0 ]; then
  PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
else
  PATH="/usr/local/bin:/usr/bin:/bin:/usr/games"
fi
export PATH=$HOME/bin:$PATH

umask 022
unsetopt nomatch

function xsel() {
    [[ -n "$DISPLAY" ]] && command xsel $@ 2>/dev/null
}

# ruby
path=(
    ~/.gem/ruby/*/bin(N-/)
    $path
)
export PATH

# perl
function _set_perl_env() {
    local arch; arch="$(perl -MConfig -e 'print $Config{archname}')"
    local extlib; extlib="$HOME/extlib/lib/perl/perl5"
    export PERL5LIB="$extlib:$extlib/$arch"
    whence cpanm >/dev/null && export PERL_CPANM_OPT="--local-lib=~/extlib"
}
whence perl >/dev/null && [[ -d ~/extlib ]] && _set_perl_env
