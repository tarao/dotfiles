# suppress suspend by C-s
stty stop undef

# history
HISTSIZE=100000
SAVEHIST=100000
setopt share_history
setopt hist_ignore_space
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# VCS
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd_vcs_info () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

typeset -ga precmd_functions
precmd_functions+=precmd_vcs_info

# alias
alias sc='screen -h 4096'
alias wcat='wget -q -O -'

#export MANPAGER='lv -c'
export MANPAGER='less -R'
export PAGER='v'
alias diff='colordiff -u'
alias od='od -A x -t xCz'

cppwarning="-Wall -Wextra -Wcast-qual -Wwrite-strings -Wno-missing-field-initializers -Wnon-virtual-dtor -Weffc++ -Wold-style-cast -Woverloaded-virtual"
alias c++="/usr/bin/g++ -std=c++98 -pedantic-errors $cppwarning"
alias c++now="/usr/bin/g++ -std=c++98 -pedantic-errors"
alias g++="/usr/bin/g++ -std=gnu++98 -pedantic $cppwarning"
alias g++now="/usr/bin/g++ -std=gnu++98 -pedantic"

alias emacs-compile='emacs -batch -f batch-byte-compile'

# prompt
PROMPT="%(!.%F{red}.%F{green})%U%n@%m%u%f:%1(j.%j.)%(!.#.>) "
RPROMPT="[%F{yellow}%~%f]%1(v|%F{blue}%1v%f|)"
