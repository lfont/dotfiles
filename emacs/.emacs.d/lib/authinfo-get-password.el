;;; authinfo-get-password --- Retrieve password from authinfo.
;;; Commentary:
;;; code:
(require 'netrc)

(defun authinfo-get-password (host port)
  "Get password for HOST and PORT."
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine netrc host port port)))
    (when hostentry (netrc-get hostentry "password"))))

(provide 'authinfo-get-password)
;;; authinfo-get-password.el ends here
