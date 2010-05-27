# suppress suspend by C-s
stty stop undef

# remove duplicated path
typeset -gxU PATH=$PATH

# history
HISTFILE=~/.zsh/history
HISTSIZE=100000
SAVEHIST=100000
setopt share_history
setopt hist_ignore_space
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# options
setopt   globdots correct pushdtohome autolist nopromptcr
setopt   nocorrectall autocd recexact longlistjobs autoresume
setopt   histignoredups pushdsilent appendhistory histexpiredupsfirst
setopt   autopushd pushdminus extendedglob rcquotes
unsetopt bgnice autoparamslash hup

# completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
autoload -U compinit
compinit

# run-help
unalias  run-help 2>/dev/null || true
autoload run-help

# functions as array
typeset -ga chpwd_functions
typeset -ga precmd_functions
typeset -ga preexec_functions

source ~/.zsh/env.zshrc
source ~/.zsh/alias.zshrc
source ~/.zsh/function.zshrc
source ~/.zsh/emacs.zshrc
source ~/.zsh/cdd.zshrc
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    source ~/.zsh/term.zshrc
else
    source ~/.zsh/term.compat.zshrc
fi
