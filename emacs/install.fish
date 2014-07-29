. ./functions/link_command.fish

mkdir -p ~/.emacs.d/
ln -sf (pwd)/emacs/init.el ~/.emacs.d/

link_command (pwd)/emacs/e.fish e
