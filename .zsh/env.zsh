export MANPAGER='less -s'
export PAGER='v'
export EDITOR='vi'

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
