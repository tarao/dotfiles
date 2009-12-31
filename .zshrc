# set display
if [ -n "$SSH_CLIENT" ]; then
    if [[ "$SSH_CLIENT" == 192.168.* ]]; then
        export DISPLAY=`echo $SSH_CLIENT | cut -d ' ' -f 1`:0.0
    fi
fi
. ~/bin/export-display

source ~/.zsh/luxaky.zshrc
source ~/.zsh/vimode.zshrc

dbl=$HOME/bin:$HOME/bin
bin=$HOME/bin
export PATH=${PATH/$dbl/$bin}

alias c++="~/bin/g++ -lstdc++ -std=c++98 -pedantic-errors $cppwarning"
alias c++now="~/bin/g++ -lstdc++ -std=c++98 -pedantic-errors"
alias g++="~/bin/g++ -lstdc++ -std=gnu++98 -pedantic $cppwarning"
alias g++now="~/bin/g++ -lstdc++ -std=gnu++98 -pedantic"

alias ii='win cygstart'
alias ck='win ck -e ssh luxaky'
alias wgui='win --interactive'
alias wresolve='win --resolve'
alias reboot='poweroff -r -w 3'
alias fx-vacuum='win C:/cygwin/bin/zsh.exe C:/home/bin/fx-vacuum'

alias xpdf='/usr/bin/xpdf -g +200+300'
alias nautilus='/usr/bin/nautilus --no-desktop -g +200+300'
alias browser='/usr/bin/nautilus --browser --no-desktop -g +200+300'
alias restartck='win startup-ck; exit'
