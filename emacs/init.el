;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el
;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup

(require 'package)
;; list the packages you want
(setq package-list '(projectile
                     flx-ido
                     magit
                     git-gutter
                     solarized-theme
                     zenburn-theme
                     tern
                     auto-complete
                     tern-auto-complete
                     fill-column-indicator
                     multiple-cursors
                     highlight-chars))

;; list the repositories containing them
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(require 'iso-transl)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;(cua-mode t)
;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;(transient-mark-mode 1) ;; No region when it is not highlighted
;(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

;; General UI stuff
(global-linum-mode t)
(global-hl-line-mode t)
(column-number-mode 1)

;(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq tab-width 2)

(setq inhibit-startup-message t)
(setq visible-bell 'top-bottom)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;(setq x-underline-at-descent-line t)
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;(setq mouse-wheel-progressive-speed nil)
;(setq mouse-wheel-follow-mouse 't)
;(setq scroll-step 1)

;(add-hook 'window-configuration-change-hook
;          (lambda ()
;            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

(load-theme 'zenburn t)
(set-default-font "Inconsolata 11")

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 0)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(require 'git-gutter)
(global-git-gutter-mode t)

(require 'fill-column-indicator)
(setq fci-rule-width 3)
(setq fci-rule-color "grey")
(add-hook 'prog-mode-hook 'fci-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-c C-<down>") 'mc/mmlte--down)
(global-set-key (kbd "C-c C-<up>") 'mc/mmlte--up)
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-k") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-u") 'mc/unmark-next-like-this)

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

;; background color bug https://github.com/bbatsov/solarized-emacs/issues/18
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))
