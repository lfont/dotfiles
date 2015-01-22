(require 'netrc)

(defun offlineimap-get-password (host port)
      (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
             (hostentry (netrc-machine netrc host port port)))
        (when hostentry (netrc-get hostentry "password"))))
