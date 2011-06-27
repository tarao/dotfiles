# VCS
autoload -Uz vcs_info
zstyle ':vcs_info:*' max-exports 4
zstyle ':vcs_info:(git|svn):*' formats '%R' '%S' '%b' '%s'
zstyle ':vcs_info:(git|svn):*' actionformats '%R' '%S' '%b|%a' '%s'
zstyle ':vcs_info:*' formats '%R' '%S' '%s:%b' '%s'
zstyle ':vcs_info:*' actionformats '%R' '%S' '%s:%b|%a' '%s'
function precmd_vcs_info () {
    psvar=()
    STY= LANG=en_US.UTF-8 vcs_info
    repos=`print -nD "$vcs_info_msg_0_"`
    if [[ -n "$vcs_info_msg_1_" ]]; then
        vcs="$vcs_info_msg_3_"
    else
        vcs=''
    fi
    [[ -n "$repos" ]] && psvar[2]="$repos"
    [[ -n "$vcs_info_msg_1_" ]] && psvar[3]="$vcs_info_msg_1_"
    [[ -n "$vcs_info_msg_2_" ]] && psvar[1]="$vcs_info_msg_2_"
}

precmd_functions+=precmd_vcs_info

# prompt
PROMPT="%(!.%F{red}.%F{green})%U%n@%6>>%m%>>%u%f:%1(j.%j.)%(!.#.>) "
local psdirs='[%F{yellow}%3(v|%32<..<%3v%<<|%60<..<%~%<<)%f]'
local psvcs='%3(v|[%25<\<<%F{yellow}%2v%f@%F{blue}%1v%f%<<]|)'
RPROMPT="$psdirs$psvcs"
