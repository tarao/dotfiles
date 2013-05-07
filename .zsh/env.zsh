export MANPAGER='less -s'
export PAGER='less -R'
whence v >/dev/null && export PAGER=`whence v`
export EDITOR='vi'
export TIME_STYLE=long-iso

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

# ls
if test -x /usr/bin/dircolors ; then
    if test -f $HOME/.dir_colors ; then
        eval "`direcolors -b $HOME/.dir_colors`"
    elif test -f /etc/DIR_COLORS ; then
        eval "`dircolors -b /etc/DIR_COLORS`"
    fi
fi
if test "$EMACS" = "t" ; then
    LS_OPTIONS='--color=none';
    test -I -Q
    stty coocked pass8 dec nl -echo
else
    LS_OPTIONS='--color=tty'
fi
LS_OPTIONS="-N -T 0 --time-style=long-iso $LS_OPTIONS"
export LS_OPTIONS
