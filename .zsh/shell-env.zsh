export MANPAGER='less -s'
export PAGER='less -R'
whence v >/dev/null && export PAGER=`whence v`
export EDITOR='vi'
export TIME_STYLE=long-iso

[ -n "$WSL_DISTRO_NAME" ] && whence wsl-open >/dev/null && {
    export BROWSER=xdg-open
}

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

# SUDO_PATH used for completion
typeset -xT SUDO_PATH sudo_path
typeset -U sudo_path
sudo_path=({/usr/local,/usr,/}/sbin(N-/))

# python
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
