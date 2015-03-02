. ./functions/link_command.fish

mkdir -p ~/.emacs.d/
ln -sf (pwd)/emacs/init.el        ~/.emacs.d/
ln -sf (pwd)/emacs/jabber.el      ~/.emacs.d/
ln -sf (pwd)/emacs/offlineimap.el ~/.emacs.d/
ln -sf (pwd)/emacs/mu4e.el        ~/.emacs.d/

mkdir -p ~/.emacs.d/lisp
ln -s (pwd)/emacs/notify.el ~/.emacs.d/lisp/
emacs --batch -f batch-byte-compile ~/.emacs.d/lisp/notify.el

ln -sf (pwd)/offlineimap/offlineimaprc  ~/.offlineimaprc
ln -sf (pwd)/offlineimap/offlineimap.py ~/.offlineimap.py

link_command (pwd)/emacs/e.sh e
link_command (pwd)/emacs/ew.sh ew
link_command (pwd)/emacs/mail-notify.fish mail-notify

sudo cp (pwd)/emacs/e.sh /root/bin/e
