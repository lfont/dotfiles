# -*-sh-*-
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
alias npm-exec='PATH=$(npm bin):$PATH'

# Curl
alias cbo="curl -X GET -L"
alias che="cbo -I"

# beets
function beet-grep-tag() {
  beet info albumartist:"$1" | grep -e "$2:" | sed -e "s,^ *,," | sort | uniq
}

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
