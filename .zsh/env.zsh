export MANPAGER='less -s'
export PAGER='less -R'
whence v >/dev/null && export PAGER=`whence v`
export EDITOR='vi'
export TIME_STYLE=long-iso

for dir in `ls -d ~/.gem/ruby/*/bin`; do
    PATH="$dir":"$PATH"
done
export PATH

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
