# -*-sh-*-

# Path to your oh-my-zsh installation.
export ZSH=/home/loic/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="fishy"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git autojump nvm zsh-syntax-highlighting history-substring-search)

# User configuration
source $ZSH/oh-my-zsh.sh

# Enable support for bash completion
autoload -U +X bashcompinit && bashcompinit

# Initialization of external tools
if command -v gpg-connect-agent >/dev/null; then
  /usr/bin/gpg-connect-agent --quiet /bye >/dev/null
fi

if command -v rbenv >/dev/null; then
  eval "$(rbenv init -)"
fi

if command -v direnv >/dev/null; then
  eval "$(direnv hook zsh)"
fi

if command -v stack >/dev/null; then
  eval "$(stack --bash-completion-script "$(which stack)")"
fi

if [ -e /usr/share/doc/pkgfile/command-not-found.zsh ]; then
  source /usr/share/doc/pkgfile/command-not-found.zsh
fi

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# zsh
alias zshconfig="x ~/.zshrc"
alias ohmyzsh="x ~/.oh-my-zsh"

# tools
alias root="sudo -i"
alias open="urxvt -e"
alias tm="tmux attach -t term || tmux new -s term"
alias tme="tmux attach -t emacs || tmux new -s emacs 'emacs -nw'"
alias tsh="cd ~/code/Turtle-Extensions && stack ghci --no-load; cd -"

# pacman (https://wiki.archlinux.org/index.php/Pacman_tips)
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

# Emacs: edit file from an Emacs shell (http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/)
if [ "${TERM}x" = "eterm-colorx" ]; then
  alias e="print -P '\eAnSiTe'"
  alias x="print -P '\eAnSiTx'"
else
  alias e=$EDITOR
  alias x=$EDITOR
fi

# Fasterize tools
function fstrz() {
  cd ~/code/Fasterize/fstrz/ && ./bin/fstrz $@; cd "$OLDPWD"
}

alias fup="fstrz host up cache01-devfe web01-devfe graphite01-devfe"
alias fdown="fstrz host halt cache01-devfe web01-devfe graphite01-devfe"
alias fssh="fstrz host ssh"
alias fzoo="fstrz zookeeper repl"
alias fmongo="fstrz mongodb repl"
alias flog="fstrz log request "
alias fcustomerup="fstrz config customer update_default -e devfe"
alias fengineup="fstrz config provision --environment devfe"

function fcustomersetattr() {
  fstrz config customer set_attr -e devfe --attribute="$1" --value="$2"
}

# Fasterize API
function fastapi() {
  local cmd="$@"
  cd ~/code/Fasterize/fastapi/ && ./shell.sh "$cmd"; cd "$OLDPWD"
}

alias fconfigtoken="fastapi configToken"
alias fconfigdownload="fastapi configDownload"

# Fastetize Capistrano
function fcustomerrestore() {
  local CONFID="/$1"
  if [ "$CONFID" = "/" ]; then
      CONFID=""
  fi
  j chef-repo \
    && cap devfe zookeeper:backup:restore_from_env -s zk_path=/config/customer_config${CONFID} -s src_env=dc1 \
    ; cd -
}

function fdbrestore() {
  case "$1" in
    zoo)
      # import ZK cache01-devfe
      fsconfrestore
      ;;
    sql-local)
      # import MySQL dans la base local
      j chef-repo \
        && cap devfe mysql:backup:local:restore_from_env -s src_env=dc1 -s password="$(pass show fasterize/mysql.local | head -n 1)" \
        ; cd -
      ;;
    sql-web01)
      # import MySQL dans la base de web01-devfe
      j chef-repo \
        && cap devfe mysql:backup:restore_from_env -s src_env=dc1 -s password="$(pass show fasterize/mysql.web01 | head -n 1)" \
        ; cd -
      ;;
    *)
      echo "usage: fsdbrestore zoo|sql-local|sql-web01"
      ;;
  esac
}
