if test -f ~/.autojump/share/autojump/autojump.fish
    . ~/.autojump/share/autojump/autojump.fish
end

complete --command root --arguments '(__fish_complete_subcommand_root -u -g)'

if status --is-login
    if test ! (which nixos-install 2>/dev/null)
        set PATH ~/.nix-profile/bin ~/.nix-profile/sbin $PATH
    end

    if not contains ~/bin $PATH
        set PATH ~/bin $PATH
    end

    set -gx EDITOR e
end
