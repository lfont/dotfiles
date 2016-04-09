;;; init-emacs.el --- Emacs misc settings
;;; Commentary:
;;; Code:

;; Always verify certificate validity
(use-package gnutls
  :init
  (setq gnutls-verify-error t))

;; Remote file access
(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "sshx")
  :config
  (add-to-list 'tramp-default-proxies-alist
               '(".*" "\\`root\\'" "/sshx:%h:")))

;; Treat asc file like gpg file
(use-package epa-file
  :init
  (setq epa-armor t
        epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  :config
  (epa-file-name-regexp-update))

;; Default browser
(use-package browse-url
  :commands browse-url-generic
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (getenv "BROWSER")))

;; Get password from authinfo.gpg
(use-package authinfo-get-password
  :load-path "~/.emacs.d/lib/"
  :commands authinfo-get-password)

(provide 'init-emacs)

;;; init-emacs.el ends here
