source ~/.zsh/vimode.zsh
source ~/.zsh/tarao.zsh
source ~/.zsh/win.zsh

alias xpdf='env xpdf -g +200+300'
alias nautilus='env nautilus --no-desktop -g +200+300'
alias browser='env nautilus --browser --no-desktop -g +200+300'
alias restartck='win startup-ck; exit'
alias restart-gterm='wautorun xsession start; exit'
