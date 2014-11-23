set PATH /usr/local/bin $PATH

if test ! (which nixos-install 2>/dev/null)
  set PATH ~/.nix-profile/bin ~/.nix-profile/sbin $PATH
end

set PATH ~/bin $PATH

if test -f ~/.autojump/share/autojump/autojump.fish
  . ~/.autojump/share/autojump/autojump.fish
end

set -e TERM
set -Ux TERM xterm-256color

if test ! (which vi 2>/dev/null)
    alias vi=vim
end
