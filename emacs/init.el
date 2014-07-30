;;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el
;;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup

(require 'package)
;; list the packages you want
(setq package-list '(projectile
                     flx-ido
                     magit
                     git-gutter
                     zenburn-theme
                     auto-complete
                     tern
                     tern-auto-complete
                     fill-column-indicator
                     multiple-cursors
                     highlight-chars
                     rainbow-delimiters
                     smex
                     php-mode
                     flycheck
                     xclip))

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
(when (display-graphic-p) (global-linum-mode t))
(global-hl-line-mode t)
(column-number-mode 1)

;; Tab behavior
(add-hook 'prog-mode-hook (lambda ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq tab-stop-list (number-sequence 4 200 4))
    (setq indent-line-function 'insert-tab)))

(setq inhibit-startup-message t)
(setq visible-bell 'top-bottom)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(load-theme 'zenburn t)
(set-frame-font "Inconsolata 11")

(add-hook 'prog-mode-hook (lambda ()
    ;(add-hook 'prog-mode-hook 'electric-indent-mode)
    (add-hook 'prog-mode-hook 'electric-pair-mode)
    (add-hook 'prog-mode-hook 'flycheck-mode)))

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
(when (display-graphic-p) (git-gutter:linum-setup))
(unless (display-graphic-p)
    (custom-set-variables
        '(git-gutter:separator-sign "|"))
    (set-face-foreground 'git-gutter:separator "grey"))

(require 'fill-column-indicator)
(setq fci-rule-width 3)
(setq fci-rule-color "grey")
(add-hook 'prog-mode-hook 'fci-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-c <down>") 'mc/mmlte--down)
(global-set-key (kbd "C-c <up>") 'mc/mmlte--up)
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c k") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c u") 'mc/unmark-next-like-this)

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)

;; Use xclip to copy/paste to the terminal from X.
(xclip-mode 1)

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                                'smarter-move-beginning-of-line)

;; http://stackoverflow.com/questions/23692879/emacs24-backtab-is-undefined-how-to-define-this-shortcut-key
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; background color bug https://github.com/bbatsov/solarized-emacs/issues/18
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

