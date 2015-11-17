;;; init.el --- user settings
;;; Commentary:
;;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el

;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/"))
(defconst my-user-lisp (expand-file-name "~/.emacs.d/site-lisp/"))

;; load modules
(defun my-load (file)
  "Load FILE from user site-lisp."
  (load (concat my-user-lisp file)))

(my-load "package")
(my-load "ui")
(my-load "editor")
(my-load "indent")
(my-load "navigation")
(my-load "helm")
(my-load "ediff")
(my-load "git")
(my-load "code")

;; autoload optional modules
(defun my-autoload (function file &optional docstring interactive)
  "Autoload file from user site-lisp when function is called.

FUNCTION is the symbol of the function.
FILE is the file to load.
DOCSTRING is an optional documentation string.
INTERACTIVE can be set if FUNCTION call be called interactively."
  (autoload function (concat my-user-lisp file) docstring interactive nil))

(my-autoload 'authinfo-get-password "authinfo-get-password")
(my-autoload 'prodigy "prodigy" "Launch prodigy." t)
(my-autoload 'multi-term "multi-term" "Launch multi-term." t)
(my-autoload 'my-jabber "jabber" "Launch jabber client." t)
(my-autoload 'my-mu4e "mu4e")
(my-autoload 'my-ispell "ispell" "Launch ispell." t)
(my-autoload 'my-window "window" "Manage windows." t)

;; autoload key bindings
(global-set-key (kbd "<f5>") 'multi-term)
(global-set-key (kbd "C-c s") 'my-ispell)
(global-set-key (kbd "C-c w") 'my-window)

;; Default browser
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))

;; Treat asc file like gpg file
(require 'epa-file)
(setq epa-armor t
      epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)

;; Accept self signed certificates
(require 'starttls)
(setq starttls-use-gnutls t
      starttls-gnutls-program  "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

;;; init.el ends here
