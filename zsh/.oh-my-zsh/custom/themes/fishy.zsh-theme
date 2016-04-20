# ZSH Theme emulating the Fish shell's default prompt.

_fishy_collapsed_wd() {
  echo $(pwd | perl -pe "
   BEGIN {
      binmode STDIN,  ':encoding(UTF-8)';
      binmode STDOUT, ':encoding(UTF-8)';
   }; s|^$HOME|~|g; s|/([^/])[^/]*(?=/)|/\$1|g
")
}

local user_color='green'; [ $UID -eq 0 ] && user_color='red'
local return_status="%{$fg_bold[red]%}%(?.. [%?])%{$reset_color%}"
PROMPT='%{$fg[$user_color]%}%n%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%} %{$fg[$user_color]%}$(_fishy_collapsed_wd)%{$reset_color%}${return_status}$(git_prompt_info)$(git_prompt_status)%{$reset_color%}%(!.#.>) '
PROMPT2='%{$fg[red]%}\ %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" ("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_ADDED="%{$fg_bold[green]%}+"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg_bold[blue]%}!"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg_bold[red]%}-"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg_bold[magenta]%}>"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg_bold[yellow]%}#"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[cyan]%}?"

add-zsh-hook precmd prompt_fishy_precmd

prompt_fishy_precmd () {
  case $TERM in
    xterm*|rxvt*|screen*)
      print -Pn "\e]0;%n@%m:%~ (%l)\a"
      ;;
    eterm-color*)
      print -P "\eAnSiTh %m"
      print -P "\eAnSiTu %n"
      print -P "\eAnSiTc %~"
      ;;
  esac
}
