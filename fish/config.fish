if status --is-login
    set PATH ~/bin /usr/local/bin $PATH
end

if test -f ~/.autojump/etc/profile.d/autojump.fish
  . ~/.autojump/etc/profile.d/autojump.fish
end

set -e TERM
set -Ux TERM xterm-256color

if test ! (which vi 2>/dev/null)
    alias vi=vim
end

