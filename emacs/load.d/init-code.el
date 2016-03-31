;;; init-code.el --- coding tools setup
;;; Commentary:
;;; Code:

;; Syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; ({[ Pairing
(use-package elec-pair
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-pair-mode))

;; Folding
(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; Project management
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

;; Extra modes
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"   . js2-mode)
         ("\\.json\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local
                              flycheck-javascript-eslint-executable
                              (flycheck-locate-config-file-ancestor-directories
                               "node_modules/.bin/eslint" nil)))))

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-doc-mode)
                                 (turn-on-haskell-indent))))

(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'init-code)

;;; init-code.el ends here
