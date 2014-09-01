. ./functions/link_command.fish

mkdir -p ~/.emacs.d/
ln -sf (pwd)/emacs/init.el ~/.emacs.d/

mkdir -p ~/.emacs.d/lisp
ln -s (pwd)/emacs/notify.el ~/.emacs.d/lisp/

ln -sf (pwd)/emacs/gnus.el ~/.gnus.el

link_command (pwd)/emacs/e.fish e
