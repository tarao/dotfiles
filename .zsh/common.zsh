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
    _list _history
autoload -U compinit
compinit

# incremental completion
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    function () { # precompile
        local A
        A=~/.zsh/modules/auto-fu/auto-fu.zsh
        [[ -e "${A:r}.zwc" ]] && [[ "$A" -ot "${A:r}.zwc" ]] ||
        zsh -c "source $A; auto-fu-zcompile $A ${A:h}" >/dev/null 2>&1
    }
    source ~/.zsh/modules/auto-fu/auto-fu; auto-fu-install
    function zle-line-init () { auto-fu-init }
    zle -N zle-line-init
    zstyle ':auto-fu:highlight' input bold
    zstyle ':auto-fu:highlight' completion fg=white
    zstyle ':auto-fu:var' postdisplay ''
    function afu+cancel () {
        afu-clearing-maybe
        ((afu_in_p == 1)) && { afu_in_p=0; BUFFER="$buffer_cur"; }
    }
    function bindkey-advice-before () {
        local key="$1"
        local advice="$2"
        local widget="$3"
        [[ -z "$widget" ]] && {
            local -a bind
            bind=(`bindkey -M main "$key"`)
            widget=$bind[2]
        }
        local fun="$advice"
        if [[ "$widget" != "undefined-key" ]]; then
            local code=${"$(<=(cat <<"EOT"
                function $advice-$widget () {
                    zle $advice
                    zle $widget
                }
                fun="$advice-$widget"
EOT
            ))"}
            eval "${${${code//\$widget/$widget}//\$key/$key}//\$advice/$advice}"
        fi
        zle -N "$fun"
        bindkey -M afu "$key" "$fun"
    }
    bindkey-advice-before "^G" afu+cancel
    bindkey-advice-before "^[" afu+cancel
    bindkey-advice-before "^J" afu+cancel afu+accept-line
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
source ~/.zsh/term.compat.zsh
[[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]] && source ~/.zsh/term.zsh
