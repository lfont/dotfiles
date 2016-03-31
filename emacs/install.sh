. ./script/functions.sh

ln -s $(pwd)/emacs/lib ~/.emacs.d/
ln -s $(pwd)/emacs/site-lisp ~/.emacs.d/
ln -s $(pwd)/emacs/load.d ~/.emacs.d/

for module in ~/.emacs.d/lib/*.el; do
  emacs --batch -f batch-byte-compile "$module"
done

ln -sf $(pwd)/offlineimap/offlineimaprc ~/.offlineimaprc
f_link_command $(pwd)/emacs/mail-notify.sh mail-notify.sh
f_link_command $(pwd)/emacs/jabber.sh jabber.sh
f_link_command $(pwd)/emacs/mail.sh mail.sh
