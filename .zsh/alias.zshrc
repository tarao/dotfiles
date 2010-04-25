alias sc='screen -h 4096'
alias wcat='wget -q -O -'

alias man='LANG=${LANG/en_US.UTF-8/en_US} env man'
alias diff='colordiff -u'
alias od='od -A x -t xCz'

function { # local scope
    local w
    w="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
    alias c++="env g++ -lstdc++ -std=c++98 -pedantic-errors $w"
    alias c++now="env g++ -lstdc++ -std=c++98 -pedantic-errors"
    alias g++="env g++ -lstdc++ -std=gnu++98 -pedantic $w"
    alias g++now="env g++ -lstdc++ -std=gnu++98 -pedantic"
}

alias emacsclient='emacsclient.emacs-snapshot'
alias emacsc='emacsclient -nw'
alias emacsd='emacs-snapshot --daemon'
alias emacs22='env emacs22'
alias emacs23='env emacs-snapshot'
alias emacs-standalone='emacs23'
function emacsb {
    env emacs-snapshot --batch $@
}
alias emacs-compile="emacsb -f batch-byte-compile"
function emacsbinstall {
    emacsb -l ~/.emacs.d/dot/install.el $@
}
function install-elisp {
    emacsbinstall --eval "(install-elisp \"$1\")"
}
function update-elisp {
    emacsinstall -f update-remote-emacs-lisp
}
