typeset -A _screen_list
function screen_list () {
    local ls; ls=`screen -ls`; local line
    foreach line in ${(f)ls}
        [[ "$line" =~ '\s*(\S+)\s+\((.*+)\)' ]] && {
            st=(${(ps:, :)match[2]})
            _screen_list[$match[1]]="$st"
            echo "$match[1] ($st)"
        }
    end
}
