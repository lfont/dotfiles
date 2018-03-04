;;; init-ui.el --- ui settings
;;; Commentary:
;;; Code:

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; General UI stuff
(setq inhibit-startup-message t
      visible-bell 'top-bottom
      use-dialog-box nil)

(global-hl-line-mode t)
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

(column-number-mode t)

;; Make more room
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode '(nil . 0))

;; Font
(add-to-list 'default-frame-alist
             '(font . "Hack-10:Normal"))

(use-package hc-zenburn-theme
  :ensure t
  :config
  (custom-set-faces
   '(hl-line ((t (:background "grey29")))))
  (load-theme 'hc-zenburn t))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (setq fci-rule-width 3
        fci-rule-color "grey29"
        fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package popwin
  :ensure t
  :init
  (setq popwin:popup-window-height 25)
  :config
  (push "*Async Shell Command*" popwin:special-display-config)
  (push '("\*Man " :regexp t) popwin:special-display-config)
  (popwin-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)
        ("C-c fT"    . treemacs)
        ("C-c ft"    . treemacs-toggle)
        ("C-c f C-t" . treemacs-find-file)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("C-c fP" . treemacs-projectile)
              ("C-c fp" . treemacs-projectile-toggle)))

(provide 'init-ui)

;;; init-ui.el ends here
