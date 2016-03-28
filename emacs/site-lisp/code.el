;;; code.el --- coding tools setup
;;; Commentary:
;;; Code:

;; Syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)

;; ({[ Pairing
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
;(require 'auto-complete)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;(require 'projectile)
(projectile-global-mode)

(require 'hook-run-once)
(hook-run-once 'find-file-hook (lambda ()
                                 (when (projectile-project-p)
                                   (load (concat my/user-lisp "git")))))

;(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook (lambda ()
                           (setq-local
                            flycheck-javascript-eslint-executable
                            (flycheck-locate-config-file-ancestor-directories
                             "node_modules/.bin/eslint" nil))))

;(require 'haskell-mode)
(add-hook 'haskell-mode-hook (lambda ()
                               (turn-on-haskell-doc-mode)
                               (turn-on-haskell-indent)))

;(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; code.el ends here
