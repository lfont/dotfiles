. ./functions/link_command.fish

mkdir -p ~/.emacs.d/
ln -sf (pwd)/emacs/init.el        ~/.emacs.d/
ln -sf (pwd)/emacs/jabber.el      ~/.emacs.d/
ln -sf (pwd)/emacs/offlineimap.el ~/.emacs.d/
ln -sf (pwd)/emacs/mu4e.el        ~/.emacs.d/

mkdir -p ~/.emacs.d/lisp
ln -s (pwd)/emacs/notify.el ~/.emacs.d/lisp/

#ln -sf (pwd)/emacs/gnus.el ~/.gnus.el

ln -sf (pwd)/offlineimap/offlineimaprc  ~/.offlineimaprc
ln -sf (pwd)/offlineimap/offlineimap.py ~/.offlineimap.py

link_command (pwd)/emacs/e.fish e
link_command (pwd)/emacs/check-new-mails.fish check-new-mails
