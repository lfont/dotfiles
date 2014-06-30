function link_application_icon
  set -l icon $argv[1]
  set -l link $argv[2]

  mkdir -p ~/.local/share/icons/hicolor/128x128/apps/

  ln -sf $icon ~/.local/share/icons/hicolor/128x128/apps/$link
  gtk-update-icon-cache ~/.local/share/icons/hicolor
end

