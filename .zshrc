source ~/.zsh/display.zshrc
source ~/.zsh/common.zshrc
source ~/.zsh/vimode.zshrc

SCREEN_TITLE_CMD_ARG=(ssh -1 man -1 v -1)

alias ii='win cygstart'
alias ck='win ck -e ssh luxaky'
alias wgui='win --interactive'
alias wresolve='win --resolve'
alias reboot='poweroff -r -w 3'
alias fx-vacuum='win C:/cygwin/bin/zsh.exe C:/home/bin/fx-vacuum'

alias xpdf='env xpdf -g +200+300'
alias nautilus='env nautilus --no-desktop -g +200+300'
alias browser='env nautilus --browser --no-desktop -g +200+300'
alias restartck='win startup-ck; exit'
alias restart-gterm='wautorun xsession start; exit'
