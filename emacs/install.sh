. ./script/functions.sh

mkdir -p ~/.emacs.d/lisp

ln -s $(pwd)/emacs/notify.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/notify.el

ln -sf $(pwd)/emacs/offlineimap.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/offlineimap.el

ln -sf $(pwd)/emacs/win-resize.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/win-resize.el

ln -sf $(pwd)/emacs/init.el        ~/.emacs.d/
ln -sf $(pwd)/emacs/init-jabber.el ~/.emacs.d/lisp/
ln -sf $(pwd)/emacs/init-mu4e.el   ~/.emacs.d/lisp/

ln -sf $(pwd)/offlineimap/offlineimaprc ~/.offlineimaprc

f_link_command $(pwd)/emacs/mail-notify.sh mail-notify
