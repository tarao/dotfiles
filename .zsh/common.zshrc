# suppress suspend by C-s
stty stop undef

# remove duplicated path
typeset -gxU PATH=$PATH

# history
HISTSIZE=100000
SAVEHIST=100000
setopt share_history
setopt hist_ignore_space
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# functions as array
typeset -ga chpwd_functions
typeset -ga precmd_functions
typeset -ga preexec_functions

source ~/.zsh/env.zshrc
source ~/.zsh/alias.zshrc
source ~/.zsh/function.zshrc
source ~/.zsh/cdd.zshrc
source ~/.zsh/term.zshrc
