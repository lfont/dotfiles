# -*-sh-*-
alias bashconfig="x ~/.bashrc"

alias ls="ls -hCF --color=auto"
alias ll="ls -hFl --color=auto"
alias la="ll -A"

alias less="less -R"
alias grep="grep --color=auto"
alias top="htop"

alias root="sudo -i"
alias tm="tmux attach -t term || tmux new -s term"
alias npm-exec='PATH=$(npm bin):$PATH'

# Edit file from an Emacs shell (http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/)
if [ "${TERM}x" = "eterm-colorx" ]; then
  alias e='echo -ne "\033AnSiTe"'
  alias x='echo -ne "\033AnSiTx"'
else
  alias e=$EDITOR
  alias x=$EDITOR
fi

# Curl
alias cbo="curl -X GET -L"
alias che="cbo -I"

# beets
function beet-grep-tag() {
  beet info albumartist:"$1" | grep -e "$2:" | sed -e "s,^ *,," | sort | uniq
}
