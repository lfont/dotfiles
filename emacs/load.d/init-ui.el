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
  (popwin-mode 1))

(provide 'init-ui)

;;; init-ui.el ends here
