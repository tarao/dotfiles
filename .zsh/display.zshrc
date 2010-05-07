if [[ ( -z "$DISPLAY" ) && ( "$SSH_CLIENT" == 192.168.* ) ]]; then
    export DISPLAY=`echo $SSH_CLIENT | cut -d ' ' -f 1`:0.0
fi

[[ -x ~/bin/export-display ]] && . ~/bin/export-display
