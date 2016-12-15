[ -s ~/.bash_colors ] && source ~/.bash_colors
[ -s ~/.bash_prompt ] && source ~/.bash_prompt

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
elif [[ -n $VISUAL ]]; then
  export EDITOR=$VISUAL
else
  export EDITOR="emacsclient -t -a mg"
fi

# Docker
export DOCKER_HOST=tcp://127.0.0.1:2375

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
[ -s ~/.gpg-agent-info ] \
  && source ~/.gpg-agent-info

# bash-completion
[ -s /usr/share/bash-completion/bash_completion ] \
  && source /usr/share/bash-completion/bash_completion

# Autojump
[ -s /etc/profile.d/autojump.bash ] \
  && source /etc/profile.d/autojump.bash

# Arch command not found
[ -s /usr/share/doc/pkgfile/command-not-found.bash ] \
  && source /usr/share/doc/pkgfile/command-not-found.bash

# NVM
[ -s /usr/share/nvm/init-nvm.sh ] \
  && source /usr/share/nvm/init-nvm.sh

# Fasterize
[ -s ~/.bash_fstrz ] \
  && source ~/.bash_fstrz

# Mimic Zsh run-help ability (Alt+h after a command to open its manpage)
bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"'

# Append sudo to command (Alt+r)
bind '"\er": "\C-asudo \C-e"'

# Initialization of external tools
if command -v gpg-connect-agent > /dev/null; then
  /usr/bin/gpg-connect-agent --quiet /bye > /dev/null
fi

if command -v rbenv > /dev/null; then
  eval "$(rbenv init -)"
fi

if command -v direnv > /dev/null; then
  eval "$(direnv hook bash)"
fi

if command -v stack > /dev/null; then
  eval "$(stack --bash-completion-script "$(command -v stack)")"
fi

# Aliases
alias bashconfig="x ~/.bashrc"

alias ls="ls -hCF --color=auto"
alias ll="ls -hFl --color=auto"
alias la="ll -A"

alias less="less -R"
alias grep="grep --color=auto"
alias top="htop"

alias root="sudo -i"
alias term="urxvt -e"
alias tm="tmux attach -t term || tmux new -s term"

# Curl
alias cbo="curl -X GET -L"
alias che="cbo -I"

# Edit file from an Emacs shell (http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/)
if [ "${TERM}x" = "eterm-colorx" ]; then
  alias e='echo -ne "\033AnSiTe"'
  alias x='echo -ne "\033AnSiTx"'
else
  alias e=$EDITOR
  alias x=$EDITOR
fi

# pacman (https://wiki.archlinux.org/index.php/Pacman_tips)
if command -v pacaur > /dev/null; then
  alias pacupg="pacaur -Syu"        # Synchronize with repositories and then upgrade packages that are out of date on the local system.
  alias pacdl="pacaur -Sw"          # Download specified package(s) as .tar.xz ball
  alias pacin="pacaur -S"           # Install specific package(s) from the repositories
  alias pacins="pacaur -U"          # Install specific package not from the repositories but from a file
  alias pacre="pacaur -R"           # Remove the specified package(s), retaining its configuration(s) and required dependencies
  alias pacrem="pacaur -Rns"        # Remove the specified package(s), its configuration(s) and unneeded dependencies
  alias pacrep="pacaur -Si"         # Display information about a given package in the repositories
  alias pacreps="pacaur -Ss"        # Search for package(s) in the repositories
  alias pacloc="pacaur -Qi"         # Display information about a given package in the local database
  alias paclocs="pacaur -Qs"        # Search for package(s) in the local database
  alias paclo="pacaur -Qdt"         # List all packages which are orphaned
  alias paclx="pacaur -Qen"         # List explicitly installed packages availble in the official repository
  alias paclxe="pacaur -Qem"        # List explicitly installed packages not avaible in the official repository
  alias pacc="pacaur -Scc"          # Clean cache - delete all the package files in the cache
  alias paclf="pacaur -Ql"          # List all files installed by a given package
  alias pacown="pacaur -Qo"         # Show package(s) owning the specified file(s)
  alias pacexpl="pacaur -D --asexp" # Mark one or more installed packages as explicitly installed
  alias pacimpl="pacaur -D --asdep" # Mark one or more installed packages as non explicitly installed
fi
