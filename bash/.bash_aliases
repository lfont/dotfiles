# -*-sh-*-
alias bashconfig="x ~/.bashrc"

alias ls="ls -hCF --color=auto"
alias ll="ls -hFl --color=auto"
alias la="ll -A"

alias less="less -R"
alias grep="grep --color=auto"
alias top="htop"

alias root="sudo -i"

# Edit file from an Emacs shell (http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/)
if [ "${TERM}x" = "eterm-colorx" ]; then
  alias e='echo -ne "\033AnSiTe"'
  alias x='echo -ne "\033AnSiTx"'
else
  alias e=$EDITOR
  alias x=$EDITOR
fi

# Tmux
alias ta="tmux attach -t"
alias tn="tmux new -s"
alias tm="ta default || tn default"
alias tl="tmux ls"

# Curl
alias cbo="curl -X GET -L"
alias che="cbo -I"

# beets
function beet-grep-tag() {
  beet info albumartist:"$1" | grep -e "$2:" | sed -e "s,^ *,," | sort | uniq
}

# Nix
alias nupdate="nix-channel --update && nix-channel --list"
alias nupgrade="nix-env -u"
alias nsearch="nix-env -qaP"
alias nsearchp="nsearch -f '<nixpkgs>' -A"
alias nlist="nix-env -qP"
alias ninstall="nix-env -iA"
alias nremove="nix-env -e"
