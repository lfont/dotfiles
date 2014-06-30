function link_command
  set -l cmd  $argv[1]
  set -l link $argv[2]

  ln -sf $cmd ~/bin/$link
end

