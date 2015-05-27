. ./script/functions.sh

mkdir -p ~/.emacs.d/lisp

ln -s $(pwd)/emacs/notify.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/notify.el

ln -sf $(pwd)/emacs/offlineimap.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/offlineimap.el

ln -sf $(pwd)/emacs/init.el        ~/.emacs.d/
ln -sf $(pwd)/emacs/init-jabber.el ~/.emacs.d/lisp/
ln -sf $(pwd)/emacs/init-mu4e.el   ~/.emacs.d/lisp/

ln -sf $(pwd)/offlineimap/offlineimaprc  ~/.offlineimaprc
ln -sf $(pwd)/offlineimap/offlineimap.py ~/.offlineimap.py

f_link_command $(pwd)/emacs/e.sh e
f_link_command $(pwd)/emacs/ew.sh ew
f_link_command $(pwd)/emacs/mail-notify.sh mail-notify

sudo mkdir -p /root/bin
sudo cp $(pwd)/emacs/e.sh /root/bin/e
