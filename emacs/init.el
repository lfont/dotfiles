;;; init.el --- user settings
;;; Commentary:
;;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Packages settings
(require 'package)
(let ((package-list '(flx-ido
                      ido-vertical-mode

                      helm
                      projectile
                      helm-projectile

                      neotree

                      magit
                      git-gutter

                      auto-complete
                      tern
                      tern-auto-complete

                      flycheck
                      flycheck-rust

                      fish-mode
                      js2-mode
                      php-mode
                      less-css-mode
                      rust-mode
                      fsharp-mode
                      haskell-mode

                      molokai-theme

                      fill-column-indicator
                      multiple-cursors
                      rainbow-delimiters

                      xclip

                      jabber)))

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
      (package-install package))))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backups
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

;; General UI stuff
(global-hl-line-mode t)
(column-number-mode t)

(setq inhibit-startup-message t)
(setq visible-bell 'top-bottom)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(load-theme 'molokai t)
(set-frame-font "Terminus-13:Regular")

;; Scroll line by line
(setq scroll-step           1
      scroll-conservatively 10000)

;; Window Resize
(global-set-key (kbd "C-c <left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c <up>")    'shrink-window)
(global-set-key (kbd "C-c <down>")  'enlarge-window)

;; Smarter move
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

;; Tab behavior
(defun set-custom-tab-behavior ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 4 200 4)))

(add-hook 'text-mode-hook 'set-custom-tab-behavior)
(add-hook 'prog-mode-hook 'set-custom-tab-behavior)
(add-hook 'css-mode-hook  'set-custom-tab-behavior)

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

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Auto indent
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; ({[ Pairing
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)

;; Use xclip to copy/paste to the terminal from X.
(require 'xclip)
(xclip-mode t)
(turn-on-xclip) ;; this function is not call in rxvt

;; Treat asc file like gpg file
(require 'epa-file)
(setq epa-armor t)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)

;; Always ALWAYS use UTF-8
(require 'iso-transl)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Spell checking
(require 'ispell)
(setq ispell-program-name "aspell"
      ispell-list-command "--list")

(let ((langs '("american" "francais")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun ispell-cycle-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode nil))))

(global-set-key (kbd "<f5>")     'ispell-word)
(global-set-key (kbd "M-<f5>")   'ispell-cycle-languages)
(global-set-key (kbd "C-S-<f5>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "C-<f5>")   'flyspell-check-next-highlighted-word)

;; Accept self signed certificates
(require 'starttls)
(setq starttls-use-gnutls t
      starttls-gnutls-program  "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

;; Saner ediff default
(require 'ediff)
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(defun command-line-merge (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left))
        (file3 (pop command-line-args-left))
        (file4 (pop command-line-args-left)))
    (ediff-merge-files-with-ancestor file1 file2 file3 nil file4)))

(add-to-list 'command-switch-alist '("merge" . command-line-merge))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'flx-ido)
;(ido-mode 1)
;(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
(require 'auto-complete)
(global-auto-complete-mode t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;(require 'linum)
;(setq linum-format " %4d |")
;(global-linum-mode)

(require 'git-gutter)
(global-git-gutter-mode)
(custom-set-variables '(git-gutter:separator-sign "|"))
(set-face-foreground 'git-gutter:separator "grey")
;(git-gutter:linum-setup)

(require 'fill-column-indicator)
(setq fci-rule-width 3)
(setq fci-rule-color "grey")
(setq fci-rule-column 80)
(add-hook 'prog-mode-hook 'fci-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-c <down>") 'mc/mmlte--down)
(global-set-key (kbd "C-c <up>")   'mc/mmlte--up)
(global-set-key (kbd "C-c l")      'mc/edit-lines)
(global-set-key (kbd "C-c d")      'mc/mark-next-like-this)
(global-set-key (kbd "C-c k")      'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c u")      'mc/unmark-next-like-this)

(require 'whitespace)
(setq whitespace-style '(face empty lines-tail tabs tab-mark trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(setq
 ;; open helm buffer in another window
 helm-split-window-default-side 'other
 ;; open helm buffer inside current window, not occupy whole other window
 helm-split-window-in-side-p t)

(helm-mode t)

(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'helm-projectile)
(defun helm-projectile-switch-buffer ()
  "Use Helm instead of ido to switch buffer in projectile."
  (interactive)
  (helm :sources helm-source-projectile-buffers-list
        :buffer "*helm projectile buffers*"
        :prompt (projectile-prepend-project-name "Switch to buffer: ")))

(require 'projectile)
(projectile-global-mode)

;; Override some projectile keymaps
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "b") 'helm-projectile-switch-buffer)
     (define-key projectile-command-map (kbd "f") 'helm-projectile)
     (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)))

(require 'fsharp-mode)
(eval-after-load 'fsharp-mode
  '(progn
     (define-key fsharp-mode-map (kbd "C-c C-SPC") 'fsharp-ac/complete-at-point)))

;; Loads some user's files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-library "notify")
(load-user-file "jabber.el")
(load-user-file "offlineimap.el")
(load-user-file "mu4e.el")

;;; init.el ends here
