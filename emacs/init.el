;;; init.el --- user settings
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/"))
(defconst my/user-lisp (expand-file-name "~/.emacs.d/site-lisp/"))

;; load modules
(defconst my/modules '("package"
                       "ui"
                       "editor"
                       "indent"
                       "helm"
                       "ediff"
                       "code"))

(dolist (module my/modules)
  (load (concat my/user-lisp module)))

;; autoload optional modules
(defconst my/autoloaded-modules '((prodigy "prodigy" "<f6>")
                                  (my/multi-term "multi-term" "<f5>")
                                  (my/jabber "jabber")
                                  (my/mu4e "mu4e")
                                  (my/spellcheck "spellcheck" "C-c s")
                                  (my/cursor "cursor" "C-c c")
                                  (my/line "line" "C-c l")
                                  (my/window "window" "C-c w")))

(dolist (module my/autoloaded-modules)
  (let ((function (car module))
        (file (nth 1 module))
        (key-binding (nth 2 module)))
    (autoload function (concat my/user-lisp file)
      "Autoloaded module from user's site-lisp"
      (if key-binding t nil)
      nil)
    (when key-binding
      (global-set-key (kbd key-binding) function))))

;; autoload library
(autoload 'authinfo-get-password "authinfo-get-password")

;; Default browser
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))

;; Accept self signed certificates
(require 'starttls)
(setq starttls-use-gnutls t
      starttls-gnutls-program  "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

;;; init.el ends here
