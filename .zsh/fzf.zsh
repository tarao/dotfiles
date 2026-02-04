[[ -d ~/.fzf ]] && {
    export PATH="$PATH:$HOME/.fzf/bin"
    export MANPATH="$MANPAH:$HOME/.fzf/man"

    export FZF_DEFAULT_OPTS="--reverse --info=inline-right"

    typeset -a FZF_FIND_FILES_EXCLUDES
    FZF_FIND_FILES_EXCLUDES=('.git' '.svn' '.hg' '.#*' '#*#' '*~')
    FZF_FIND_FILES_TOGGLE_KEY='ctrl-s'
    function _fzf_files_excludes () {
        echo "${(F)FZF_FIND_FILES_EXCLUDES}"
        local root=$(git rev-parse --show-toplevel 2>/dev/null)
        [[ -z "$root" ]] && return
        [[ -r "$root/.gitignore" ]] || return
        cat "$root/.gitignore"
    }
    function _fzf_files_handle () {
        local query key item
        read query
        read key
        [[ -z "$key" ]] && { # Enter
            while read item; do
                local nolink="${item% -> *}"
                [[ -h "$nolink" ]] && item="$nolink"
                echo -n "${(q)item} "
            done
            return
        }

        # switch
        case "$key" in
        alt-f) _fzf_files file "$query" ;;
        alt-a) _fzf_files all "$query" ;;
        "$FZF_FIND_FILES_TOGGLE_KEY")
            case "$1" in
            file) _fzf_files all "$query" ;;
            all) _fzf_files file "$query" ;;
            esac
            ;;
        esac
    }
    function _fzf_files () {
        local query="$2"
        local t="${1:-file}"
        local option="-ifC --noreport --charset=C"
        [[ "$t" = 'all' ]] && option="$option -a"
        local excludes=($(_fzf_files_excludes))
        (( $#excludes > 0 )) && option="$option -I '${(j:|:)excludes}'"
        eval "command tree $option | tail -n +2 | perl -pnle 's!^([\\[0-9;m]+)?[.]/!\1!'" | \
            fzf --ansi -m --expect=alt-f,alt-a,"$FZF_FIND_FILES_TOGGLE_KEY" --print-query -q "$query" | \
            _fzf_files_handle "$t"
    }
    function fzf-find-file-widget () {
        LBUFFER="$LBUFFER$(_fzf_files)"
        zle redisplay
    }
    zle -N fzf-find-file-widget

    FZF_HISTORY_TOGGLE_KEY='ctrl-r'
    function _fzf_history_filter () {
        perl -pnle 's/^( *[0-9*]+)( +)(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2})(.*)$/\1\2[1;30m\3 \4[m\5/'
    }
    function fzf-history-widget () {
        local output selected num
        output=$(fc -lid 1 | _fzf_history_filter | fzf +s --ansi --tac +m --with-nth=2.. --tiebreak=index --toggle-sort="$FZF_HISTORY_TOGGLE_KEY" ${=FZF_CTRL_R_OPTS} --expect=ctrl-e -q "${LBUFFER//$/\\$}")
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

        local cols=$(tput cols)
        local r root_length=0
        for r in "$roots[@]"; do
            (( $root_length < $#r )) && root_length=$#r
        done

        local repository components sep1 sep2
        while read repository; do
            repository="$(print -nD "$repository")"
            for r in "$roots[@]"; do
                r="${r%/}/"
                components=(${(s:/:)repository#$r})
                sep1="${(j:/:)components[2,-1]}"
                sep1=$(( $cols - $root_length - 6 - $#components[1] - $#sep1 ))
                sep1=$(printf ' '%.0s {1..$sep1})
                sep2=$(( $root_length + 1 - ($#r - 1) ))
                sep2=$(printf ' '%.0s {1..$sep2})
                [[ "$repository" = "$r"* ]] && {
                    echo "$r ${repository#$r} ${(j:[34m/[m:)components[2,-1]}$sep1[36m$components[1][m$sep2[1;30m[${r%/}][m"
                    break
                }
            done
        done
    }
    function _fzf_ghq_worktree_list () {
        local repo_path="$1"
        (cd "$repo_path" && git-wt 2>/dev/null | tail -n +2 | \
            awk '{
                # Skip * marker if present
                if ($1 == "*") {
                    # * PATH BRANCH HEAD
                    printf "%s\t%s\t%s\n", $3, $2, $4
                } else {
                    # PATH BRANCH HEAD
                    printf "%s\t%s\t%s\n", $2, $1, $3
                }
            }')
    }
    function _fzf_ghq_worktree_handle () {
        local line
        line=$(tail -1)

        # Empty selection means user cancelled
        [[ -z "$line" ]] && return 1

        # Extract branch name (first field before tab)
        local branch="${line%%$'\t'*}"

        # Generate git-wt command
        echo "git wt \"$branch\""
    }
    function _fzf_ghq_worktree_select () {
        local repo_path="$1"

        # Condition 1: Check if git-wt is installed
        whence git-wt >/dev/null || return 1

        # Condition 2: Check if repository has 3+ lines (header + 2+ worktrees)
        local wt_output
        wt_output=$(cd "$repo_path" && git-wt 2>/dev/null)
        [[ $? -ne 0 ]] && return 1

        local line_count=$(wc -l <<< "$wt_output")
        [[ $line_count -lt 3 ]] && return 1

        # Show worktree selection with branch names only
        local cmd
        cmd=$(_fzf_ghq_worktree_list "$repo_path" | \
            fzf +s --ansi --delimiter=$'\t' --with-nth=1 \
                --prompt="Worktree> " --height=40% | \
            _fzf_ghq_worktree_handle)

        [[ -z "$cmd" ]] && return 1

        echo "cd \"$repo_path\" && $cmd"
    }
    function fzf-ghq-widget () {
        local -a repository=($(ghq list -p 2>/dev/null | _fzf_ghq_filter | fzf +s --ansi --with-nth=3.. -q "${BUFFER//$/\\$}"))
        [[ -n "$repository" ]] && {
            local repo_path="${(j::)repository[1,2]}"
            # Expand tilde to actual home directory path
            repo_path="${repo_path/#\~/$HOME}"

            # Try worktree selection first
            local cmd=$(_fzf_ghq_worktree_select "$repo_path")

            # Fallback to simple cd if not applicable
            [[ -z "$cmd" ]] && cmd="cd \"$repo_path\""

            BUFFER="$cmd"
            zle accept-line
        }
        zle redisplay
    }
    zle -N fzf-ghq-widget

    function _fzf_git_branch_local () {
        git for-each-ref --sort=-authordate refs/heads --format='%(color:dim yellow)%(objectname:short)%(color:reset) %(refname:short) %(color:dim cyan)(%(authorname))%(color:reset) %(color:dim white)%(contents:subject)%(color:reset)%09%(color:dim cyan)%(authordate:short)%(color:reset)'
    }
    function _fzf_git_branch_remote () {
        git for-each-ref --sort=-authordate refs/remotes --format='%(color:dim yellow)%(objectname:short)%(color:reset) %(refname:short) %(color:dim cyan)(%(authorname))%(color:reset) %(color:dim white)%(contents:subject)%(color:reset)%09%(color:dim cyan)%(authordate:short)%(color:reset)'
    }
    function _fzf_git_branch_handle () {
        local query key cmd
        local -a item
        read query
        read key

        [[ -z "$key" ]] && {
            item=($(tail -1))
            local option
            [[ "$1" == 'remote' ]] && option=" -b ${item[2]#*/} -t"
            echo "git checkout$option $item[2]"
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
        ctrl-l) _fzf_git_branch local "$query" ;;
        ctrl-r) _fzf_git_branch remote "$query" ;;
        esac
    }
    function _fzf_git_branch () {
        local query="$2"
        local t="${1:-local}"
        local cmd
        case "$t" in
        local)  cmd='_fzf_git_branch_local' ;;
        remote) cmd='_fzf_git_branch_remote' ;;
        esac
        eval "$cmd" | fzf +s --ansi -m --expect=ctrl-l,ctrl-r,ctrl-x,ctrl-c --print-query -q "$query" | \
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
