if [ -n "$SSH_CLIENT" ]; then
    if [[ "$SSH_CLIENT" == 192.168.* ]]; then
        export DISPLAY=`echo $SSH_CLIENT | cut -d ' ' -f 1`:0.0
    fi
fi
. ~/bin/export-display
