function link_desktop_file
  set -l desktop $argv[1]

  ln -sf $desktop ~/.local/share/applications/
end

