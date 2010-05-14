export MANPAGER='less -s'
export PAGER='v -e'

if test -x /usr/bin/dircolors ; then
    if test -f $HOME/.dir_colors ; then
        eval "`direcolors -b $HOME/.dir_colors`"
    elif test -f /etc/DIR_COLORS ; then
        eval "`dircolors -b /etc/DIR_COLORS`"
    fi
fi
LS_OPTIONS=--color=tty
LS_OPTIONS="-N $LS_OPTIONS -T 0"
if test "$EMACS" = "t" ; then
    LS_OPTIONS='-N --color=none -T 0';
    test -I -Q
    stty coocked pass8 dec nl -echo
fi
export LS_OPTIONS
