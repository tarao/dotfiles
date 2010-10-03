# Fork of cdd by Yuichi Tateno
# See: http://d.hatena.ne.jp/secondlife/20080218/1203303528

export CDD_PWD_FILE=$HOME/.zsh/cdd_pwd_list

function _cdd_pwd_file() {
    if [ ! -f "$CDD_PWD_FILE" ]; then
        echo "\n" >> "$CDD_PWD_FILE"
        touch "$CDD_PWD_FILE.t"
        if [ $? = 1 ]; then
            echo "cdd: cannot write '$CDD_PWD_FILE'." > /dev/stdrr
            return 1
        fi
        chmod 600 "$CDD_PWD_FILE"
        chmod 600 "$CDD_PWD_FILE.t"
    fi
}

function _reg_pwd_screennum() {
    [ "$STY" != "" ] && _reg_cdd_pwd "$WINDOW" "$PWD"
}

function _reg_cdd_pwd() {
    _cdd_pwd_file
    [ $? = 1 ] && return 1
    sed -i".t" -e "/^$1:/d" "$CDD_PWD_FILE"
    sed -i".t" -e "1i \\
$1:$2" "$CDD_PWD_FILE"
}

function _cdadd() {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo "Usage: cdd add <name> <path>"
        echo "Example: cdd add w ~/myworkspace"
        return 1
    fi

    local real_path
    real_path="${2:a}"
    echo "add $1:$real_path"
    _reg_cdd_pwd "$1" "$real_path"
}

function _cddel() {
    if [ -z "$1" ]; then
        echo "Usage: cdd del <name>"
        return 1
    fi
    sed -i".t" -e "/^$1:/d" "$CDD_PWD_FILE"
}

function cdd() {
    if [ "$1" = "add" ]; then
        shift
        _cdadd $@
        return 0
    elif [ "$1" = "del" ]; then
        shift
        _cddel $@
        return 0
    fi

    local arg
    arg=`echo $1|cut -d':' -f1`
    if command grep "^$arg:" "$CDD_PWD_FILE" > /dev/null 2>&1 ; then
        local res
        res=`command grep "^$arg:" "$CDD_PWD_FILE"|sed -e "s/^$arg://;"|tr -d "\n"`
        echo "$res"
        cd "$res"
    else
        sed -e '/^$/d' "$CDD_PWD_FILE"
    fi
}

compctl -K _cdd cdd
functions _cdd() {
    reply=(`command grep -v "^$WINDOW:" "$CDD_PWD_FILE"`)
}

chpwd_functions+=_reg_pwd_screennum
