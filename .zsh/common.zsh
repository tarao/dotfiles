# suppress suspend by C-s
stty stop undef

# remove duplicated path
typeset -gxU PATH=$PATH

# history
HISTFILE=~/.zsh/history
HISTSIZE=100000
SAVEHIST=100000
setopt   share_history append_history
setopt   hist_ignore_space hist_ignore_dups hist_expire_dups_first
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# options
## Changing Directories
setopt   auto_pushd pushd_to_home pushd_minus pushd_silent auto_cd
## Expansion and Globbing
setopt   extended_glob glob_dots
## Input/Output
setopt   correct rc_quotes
unsetopt correct_all
## Job Control
setopt   long_list_jobs auto_resume
unsetopt bg_nice hup
## Prompting
unsetopt prompt_cr
## Zle
unsetopt beep

# run-help
unalias  run-help 2>/dev/null || true
autoload run-help

# functions as array
typeset -ga chpwd_functions
typeset -ga precmd_functions
typeset -ga preexec_functions

# utilities
autoload -Uz is-at-least

source ~/.zsh/env.zsh
source ~/.zsh/alias.zsh
source ~/.zsh/complete.zsh
source ~/.zsh/function.zsh
source ~/.zsh/emacs.zsh
source ~/.zsh/cdd.zsh
source ~/.zsh/term.compat.zsh
is-at-least 4.3.10 && source ~/.zsh/term.zsh
is-at-least 4.3.10 && source ~/.zsh/screen.zsh
is-at-least 4.3.10 && source ~/.zsh/screen-title.zsh
is-at-least 4.3.10 && source ~/.zsh/screen-attach.zsh
source ~/.zsh/screen-alias.zsh
