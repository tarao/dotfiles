function whole_display_size () {
    local MATCH_PCRE=1
    local dimensions=$(xdpyinfo | grep dimensions)
    [[ "$dimensions" =~ '[^0-9]*([0-9]+)x([0-9]+) pixels .*' ]] && {
        echo $match[1,2]
        return
    }
    return 1
}

function display_dimensions () {
    local MATCH_PCRE=1
    local line
    local d='([0-9]+)'
    local p="[a-zA-Z0-9_-]+ connected( primary)? ${d}x${d}[+]${d}[+]${d} .*"
    xrandr --prop | while read line; do
        [[ "$line" =~ "$p" ]] && {
            local -a r=($match[2] $match[3] $match[4] $match[5] $match[1])
            echo $r
        }
    done
}

function display_rects () {
    local -a whole_size=($(whole_display_size))
    local -a dimensions
    display_dimensions | while read -A dimensions; do
        local left="$dimensions[3]"
        local top="$dimensions[4]"
        local width="$dimensions[1]"
        local height="$dimensions[2]"
        local right=$(( $whole_size[1] - $left - $width ))
        local bottom=$(( $whole_size[2] - $top - $height ))
        local -a r=($left $top $right $bottom $width $height)
        echo $r
    done
}

function largest_display_rect () {
    local -a largest
    local -a rect
    display_rects | while read -A rect; do
        rect[7]=$(( $rect[5] * $rect[6] ))
        (( $rect[7] > ${largest[7]:-0} )) && {
            largest=($rect[@])
        }
    done
    [[ -z $largest[7] ]] && return 1
    echo $largest
}
