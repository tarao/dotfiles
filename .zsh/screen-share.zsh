function screen_share () {
    [[ -z "$1" ]] && {
        echo "Usage: $0 <session> <user1> <user2> ...."
        return 0
    }

    local rc; rc="$HOME/.screen/share.screenrc"
    [[ -e "$rc" ]] || {
        echo "'$rc' does not exist"
        return 1
    }

    local session; session="$1"; shift
    SHARE_USERS="${(j:,:)@}" screen -S "$session" -c "$rc"
}
