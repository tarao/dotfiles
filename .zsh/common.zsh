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

# completion
setopt   auto_list auto_param_slash list_packed rec_exact
unsetopt list_beep
zstyle ':completion:*' menu select
zstyle ':completion:*' format '%F{white}%d%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' remote-access false
zstyle ':completion:*' completer _oldlist _complete _match _ignored \
    _approximate _list _history
autoload -U compinit
compinit

# incremental completion
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    source ~/.zsh/auto-fu.zsh
    function () {
        local code
        code=${functions[auto-fu-init]/'\n-azfu-'/''}
        eval "function auto-fu-init () { $code }"
        code=${functions[auto-fu]/fg=black,bold/fg=white}
        eval "function auto-fu () { $code }"
    }
    function zle-line-init () {
        auto-fu-init
    }
    function afu+cancel () {
        afu-clearing-maybe
        ((afu_in_p == 1)) && { afu_in_p=0; BUFFER="$buffer_cur" }
    }
    function afu-bindkey-advice-before () {
        local key="$1"
        local advice="$2"
        local -a bind
        bind=(`bindkey -M main "$key"`)
        local widget=$bind[2]
        local fun="$advice"
        if [[ "$widget" != "undefined-key" ]]; then
            local code=${"$(<=(cat <<"EOT"
                (( $+functions[afu+$widget] )) || function afu+$widget () {
                    zle $advice
                    zle $widget
                }
                fun="afu+$widget"
EOT
            ))"}
            eval "${${${code//\$widget/$widget}//\$key/$key}/\$advice/$advice}"
        fi
        zle -N "$fun"
        bindkey -M afu "$key" "$fun"
    }
    afu-bindkey-advice-before "^G" afu+cancel
    afu-bindkey-advice-before "^[" afu+cancel
    zle -N zle-line-init
fi

# run-help
unalias  run-help 2>/dev/null || true
autoload run-help

# functions as array
typeset -ga chpwd_functions
typeset -ga precmd_functions
typeset -ga preexec_functions

source ~/.zsh/env.zsh
source ~/.zsh/alias.zsh
source ~/.zsh/function.zsh
source ~/.zsh/emacs.zsh
source ~/.zsh/cdd.zsh
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    source ~/.zsh/term.zsh
else
    source ~/.zsh/term.compat.zsh
fi