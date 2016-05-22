[[ -d ~/.fzf ]] && {
    export PATH="$PATH:$HOME/.fzf/bin"
    export MANPATH="$MANPAH:$HOME/.fzf/man"

    export FZF_DEFAULT_OPTS="--reverse --inline-info"

    function _fzf_git_ls_files () {
        git rev-parse HEAD >/dev/null || return 1
        git ls-files
    }
    function _fzf_find_files () {
        local option="-path '*/*~' -o -path '*/#*#' -o "
        [[ "$1" != 'all' ]] && option="$option -path '*/\\.*' -o "
        eval "command find -L . \
                \\( ${option}-fstype 'dev' -o -fstype 'proc' \\) -prune \
                -o -type f -print \
                -o -type d -print \
                -o -type l -print 2> /dev/null | sed 1d | cut -b3-"
    }
    function _fzf_files_default_type () {
        local t='file'
        git rev-parse HEAD >/dev/null 2>&1 && t='git'
        echo "$t"
    }
    function _fzf_files_handle () {
        local key item
        read key
        [[ -z "$key" ]] && { # Enter
            while read item; do
                echo -n "${(q)item} "
            done
            return
        }

        # switch
        case "$key" in
        alt-f) _fzf_files file ;;
        alt-a) _fzf_files all ;;
        alt-g) _fzf_files git ;;
        ctrl-s)
            case "$1" in
            file) _fzf_files all ;;
            all)  _fzf_files git ;;
            git)  _fzf_files file ;;
            esac
            ;;
        esac
    }
    function _fzf_files () {
        local t="${1:-$(_fzf_files_default_type)}"
        local cmd
        case "$t" in
        file) cmd='_fzf_find_files file' ;;
        all)  cmd='_fzf_find_files all' ;;
        git)  cmd='_fzf_git_ls_files' ;;
        esac
        eval "$cmd" | fzf -m --expect=alt-f,alt-a,alt-r,ctrl-s | \
            _fzf_files_handle "$t"
    }
    function fzf-find-file-widget () {
        LBUFFER="$LBUFFER$(_fzf_files)"
        zle redisplay
    }
    zle -N fzf-find-file-widget

    function _fzf_history_filter () {
        perl -pnle '$_ =~ s/^( *[0-9*]+)( +)(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2})(.*)$/\1\2[1;30m\3 \4[m\5/'
    }

    function fzf-history-widget () {
        local output selected num
        output=$(fc -lid 1 | _fzf_history_filter | fzf +s --ansi --tac +m --with-nth=2.. --tiebreak=index --toggle-sort=ctrl-r ${=FZF_CTRL_R_OPTS} --expect=ctrl-e -q "${LBUFFER//$/\\$}")
        key=$(head -1 <<< "$output")
        selected=($(head -2 <<< "$output" | tail -1))
        if [ -n "$selected" ]; then
            num=$selected[1]
            if [ -n "$num" ]; then
                zle vi-fetch-history -n $num
                [[ "$key" = 'ctrl-e' ]] || zle accept-line
            fi
        fi
        zle redisplay
    }
    zle -N fzf-history-widget

    function _fzf_ghq_filter () {
        local -a roots
        roots=($(git config --get-all ghq.root))
        (( $#roots < 1 )) && roots[1]='~/.ghq'

        local repository r components
        while read repository; do
            repository="$(print -nD "$repository")"
            for r in "$roots[@]"; do
                r="${r%/}/"
                components=(${(s:/:)repository#$r})
                [[ "$repository" = "$r"* ]] && {
                    echo "$r ${repository#$r} ${(j:[34m/[m:)components[2,-1]}	[36m$components[1][m	[1;30m[${r%/}][m"
                    break
                }
            done
        done
    }

    function fzf-ghq-widget () {
        local -a repository=($(ghq list -p 2>/dev/null | _fzf_ghq_filter | fzf +s --ansi --with-nth=3.. -q "${BUFFER//$/\\$}"))
        [[ -n "$repository" ]] && {
            BUFFER="cd ${(j::)repository[1,2]}"
            zle accept-line
        }
        zle redisplay
    }
    zle -N fzf-ghq-widget

    function _fzf_git_branch_local () {
        git for-each-ref --sort=-committerdate refs/heads --format='%(color:dim yellow)%(objectname:short)%(color:reset) %(refname:short) %(color:dim cyan)(%(authorname))%(color:reset) %(color:dim white)%(contents:subject)%(color:reset)%09%(color:dim cyan)%(committerdate:short)%(color:reset)'
    }
    function _fzf_git_branch_remote () {
        git for-each-ref --sort=-committerdate refs/remotes --format='%(color:dim yellow)%(objectname:short)%(color:reset) %(refname:short) %(color:dim cyan)(%(authorname))%(color:reset) %(color:dim white)%(contents:subject)%(color:reset)%09%(color:dim cyan)%(committerdate:short)%(color:reset)'
    }
    function _fzf_git_branch_handle () {
        local key cmd
        local -a item
        read key

        [[ -z "$key" ]] && {
            item=($(tail -1))
            local option
            [[ "$1" == 'remote' ]] && option=' -t'
            echo "git checkout$option -- $item[2]"
            return
        }

        [[ "$key" = 'ctrl-c' ]] && {
            case "$1" in
            remote)
                local -a remotes
                remotes=($(git remote))
                echo "git remote prune -- $remotes"
                ;;
            local)
                # remove local branches without upstream or with
                # invalid upstreams
                local -a branches
                local branch
                git for-each-ref --format='%(refname:short) %(upstream)' refs/heads | while read -A item; do
                    [[ -n "$item[2]" ]] && git rev-parse "$item[2]" >/dev/null 2>&1 && continue
                    branch="$item[1]"
                    branches=($branches ${(q)branch})
                done
                (( $#branches > 0 )) && echo "git branch -d $branches"
                ;;
            esac
            return
        }

        [[ "$key" = 'ctrl-x' ]] && {
            local -a refs
            while read -A item; do
                refs=($refs $item[2])
            done

            case "$1" in
            remote)
                local -A remotes
                local ref remote branch
                for ref in "$refs[@]"; do
                    remote="${ref%%/*}"
                    branch="${ref#*/}"
                    remotes[$remote]="$remotes[$remote] ${(q)branch}"
                done
                read -q \?"${(F)refs}

Are you sure to remove these remote branches and their tracking branches? (y/n) " || return
                for remote in "${(@k)remotes}"; do
                    echo -n "git push ${(q)remote} --delete $remotes[$remote]; "
                done
                ;;
            local)
                echo "git branch -d $refs"
                ;;
            esac
            return
        }

        # switch
        case "$key" in
        ctrl-l) _fzf_git_branch local ;;
        ctrl-r) _fzf_git_branch remote ;;
        esac
    }
    function _fzf_git_branch () {
        local t="${1:-local}"
        local cmd
        case "$t" in
        local)  cmd='_fzf_git_branch_local' ;;
        remote) cmd='_fzf_git_branch_remote' ;;
        esac
        eval "$cmd" | fzf +s --ansi -m --expect=ctrl-l,ctrl-r,ctrl-x,ctrl-c | \
            _fzf_git_branch_handle "$t"
    }
    function fzf-git-branch-widget () {
        local cmd=$(_fzf_git_branch)
        [[ -n "$cmd" ]] && {
            BUFFER="$cmd"
            zle accept-line
        }
        zle redisplay
    }
    zle -N fzf-git-branch-widget
}
