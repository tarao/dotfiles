alias ii='win cygstart'
alias ck='win ck -e ssh luxaky'
alias wgui='win --interactive'
alias wresolve='win --resolve'
alias reboot='poweroff -r -w 3'
alias fx-vacuum='win C:/cygwin/bin/zsh.exe C:/home/bin/fx-vacuum'

function wautorun () {
    win c:/home/bin/autorun.bat $@ >/dev/null
}
function wdrives () {
    win fsutil fsinfo drives | nkf -w | sed -e 1d -e 's/:\\/: /g'
}
