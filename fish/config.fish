set PATH /sbin /usr/sbin /usr/local/bin $PATH

if test ! (which nixos-install 2>/dev/null)
  set PATH ~/.nix-profile/bin ~/.nix-profile/sbin $PATH
end

set PATH ~/bin $PATH

if test -f ~/.autojump/share/autojump/autojump.fish
  . ~/.autojump/share/autojump/autojump.fish
end
