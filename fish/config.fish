if status --is-login
    set PATH ~/bin /usr/local/bin $PATH
end

if test -f ~/.autojump/etc/profile.d/autojump.fish
  . ~/.autojump/etc/profile.d/autojump.fish
end

