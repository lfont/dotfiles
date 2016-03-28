;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; General UI stuff
(global-hl-line-mode t)
(column-number-mode t)
(menu-bar-mode -1)

(setq inhibit-startup-message t
      visible-bell 'top-bottom
      use-dialog-box nil)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(load-theme 'molokai t)
(add-to-list 'default-frame-alist
             '(font . "Hack-10:Normal"))

;; Fix a weird bug with dead keys when Emacs runs in a GUI
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (global-set-key (kbd "<dead-acute> <SPC>")        "'")
            (global-set-key (kbd "<dead-grave> <SPC>")        "`")
            (global-set-key (kbd "<S-dead-tilde> <SPC>")      "~")
            (global-set-key (kbd "<S-dead-diaeresis> <SPC>")  "\"")
            (global-set-key (kbd "<S-dead-circumflex> <SPC>") "^")))

(require 'fill-column-indicator)
(setq fci-rule-width 3
      fci-rule-color "grey"
      fci-rule-column 80)
(add-hook 'prog-mode-hook 'fci-mode)

(require 'whitespace)
(setq whitespace-style '(face empty lines-tail tabs tab-mark trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'spaceline-config)
(spaceline-emacs-theme)
(setq powerline-height 18
      powerline-default-separator 'wave)

(require 'popwin)
(popwin-mode 1)
(setq popwin:popup-window-height 25)
(push "^term<" popwin:special-display-config)
