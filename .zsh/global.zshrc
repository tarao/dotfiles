# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

PS1='%B%n@%m:%~>%b '

# Environment
HISTSIZE=1000
HISTFILE=${HOME}/.zsh_history
SAVEHIST=500

# Set/unset  shell options
setopt   globdots nocorrect pushdtohome autolist nopromptcr
setopt   nocorrectall autocd recexact longlistjobs autoresume
setopt   histignoredups pushdsilent appendhistory histexpiredupsfirst
setopt   autopushd pushdminus extendedglob rcquotes
unsetopt bgnice autoparamslash hup

# Setup some basic programmable completions.  To see more examples
# of these, check out /usr/doc/packages/zsh/compctl-examples.
# You you have a slow machine, you might want to comment the lines below
# with compctl in, and comment the below two lines out.
#
# compctl -g '*(-/)' cd pushd
# compctl -g '*(/)' rmdir dircmp
# compctl -j -P % -x 's[-] p[1]' -k signals -- kill
# compctl -j -P % fg bg wait jobs disown
# compctl -A shift
# compctl -caF type whence which
# compctl -F unfunction
# compctl -a unalias
# compctl -v unset typeset declare vared readonly export integer
# compctl -e disable
# compctl -d enable
#
# The default way is the usage of the zsh built-in completer
# Comment the two below lines out, if you are on a slow machine, and
# comment the above compctl lines in.

autoload -U compinit
compinit

# Online help
unalias  run-help 2>/dev/null || true
autoload run-help

# History completion
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# Colored listings
if test -x /usr/bin/dircolors ; then
  if test -f $HOME/.dir_colors ; then
    eval "`direcolors -b $HOME/.dir_colors`"
  elif test -f /etc/DIR_COLORS ; then
    eval "`dircolors -b /etc/DIR_COLORS`"
  fi
fi
LS_OPTIONS=--color=tty
LS_OPTIONS="-N $LS_OPTIONS -T 0"
# Emacs shell mode
if test "$EMACS" = "t" ; then
  LS_OPTIONS='-N --color=none -T 0';
  test -I -Q
  stty coocked pass8 dec nl -echo
fi
export LS_OPTIONS

# Aliases
alias ls='/bin/ls -F $=LS_OPTIONS'
alias dir='ls -l'
alias la='ls -la'
alias ll='ls -alF'
alias ..='cd ..'
alias ...='cd ../..'
alias -- +='pushd .'
alias -- -='popd'
