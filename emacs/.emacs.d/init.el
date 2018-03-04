;;; init.el --- user settings
;;; Commentary:
;;; Code:

(require 'server)
(unless (server-running-p)
  (server-start))

;; setup package.el
(require 'package)
(setq package-enable-at-startup nil)

;; list the repositories containing packages
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; load user's settings
(use-package load-dir
  :ensure t
  :init
  (setq load-dirs t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:separator-sign "|")
 '(package-selected-packages
   (quote
    (dockerfile-mode treemacs-projectile treemacs hindent intero-mode intero flycheck-elm toml-mode markdown-mode+ yaml-mode swiper ivy fsharp-mode omnisharp csharp-mode elm-mode typescript-mode tern flycheck company mu4e-alert rbenv nvm xclip web-mode use-package tide spaceline smex rainbow-delimiters purescript-mode psc-ide projectile prodigy popwin nix-mode multiple-cursors modalka minibuffer-line magit load-dir json-mode js2-mode ivy-hydra hc-zenburn-theme haskell-mode git-gutter fill-column-indicator exwm elfeed counsel company-tern company-quickhelp ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey29"))))
 '(minibuffer-line ((t (:height 1.0 :weight regular :foreground "LightBlue3")))))
