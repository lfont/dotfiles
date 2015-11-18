;; Syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)

;; ({[ Pairing
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
;(require 'auto-complete)
(global-auto-complete-mode t)

;(require 'projectile)
(projectile-global-mode)

;(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;(require 'haskell-mode)
(add-hook 'haskell-mode-hook (lambda ()
                               (turn-on-haskell-doc-mode)
                               (turn-on-haskell-indent)))

;(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
