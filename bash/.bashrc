[ -s ~/.bash_colors ] && source ~/.bash_colors
[ -s ~/.bash_prompt ] && source ~/.bash_prompt
[ -s ~/.bash_aliases ] && source ~/.bash_aliases

# Preferred editor
if [[ -n $VISUAL ]]; then
  export EDITOR=$VISUAL
else
  if command -v mg > /dev/null; then
    export EDITOR=mg
  elif command -v nano > /dev/null; then
    export EDITOR=nano
  else
    export EDITOR=vi
  fi

  if command -v emacsclient > /dev/null; then
    export EDITOR="emacsclient -t -a $EDITOR"
  fi
fi

# Store only one copy of each command
export HISTCONTROL="erasedups:ignoreboth"

# Store more commands
export HISTFILESIZE=500000
export HISTSIZE=100000

# Ignore exit from history
export HISTIGNORE="&:[ ]*:exit"

# Dot not overwrite history on exit
shopt -s histappend

# Save multiline command as one command
shopt -s cmdhist

# Auto cd
shopt -s autocd

# Checks the window size after each command
shopt -s checkwinsize

# GPG
export GPG_TTY=$(tty)
if [ -s ~/.gpg-agent-info ]; then
  source ~/.gpg-agent-info
else
  unset SSH_AGENT_PID
  if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    #export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
  fi
fi

# bash-completion
[ -s /usr/share/bash-completion/bash_completion ] \
  && source /usr/share/bash-completion/bash_completion

# Autojump
[ -s /usr/share/autojump/autojump.bash ] \
  && source /usr/share/autojump/autojump.bash

# Mimic Zsh run-help ability (Alt+h after a command to open its manpage)
bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"'

# Append sudo to command (Alt+r)
bind '"\er": "\C-asudo \C-e"'

# Initialization of external tools
if command -v gpg-connect-agent > /dev/null; then
  /usr/bin/gpg-connect-agent --quiet /bye > /dev/null
fi
