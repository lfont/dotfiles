;;; init.el --- my settings
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
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;; Global libraries
(use-package diminish)
(use-package bind-key)
(use-package s)

;;; Emacs misc settings

;; Always verify certificate validity
(use-package gnutls
  :init
  (setq gnutls-verify-error t))

;; Remote file access
(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "sshx"))
  ;; :config
  ;; (add-to-list 'tramp-default-proxies-alist
  ;;              '(".*" "root" "/sshx:%h:")))

;; Treat asc file like gpg file
(use-package epa-file
  :ensure nil
  :init
  (setq epa-armor t
        epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  :config
  (epa-file-name-regexp-update))

;; Default browser
(use-package browse-url
  :commands browse-url-generic
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (getenv "BROWSER")))

;; Get password from authinfo.gpg
(use-package authinfo-get-password
  :load-path "~/.emacs.d/lib/"
  :commands authinfo-get-password)

;; File browser
(use-package dired
  :ensure nil
  :commands dired
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)
              ("z" . my/dired-dotfiles-toggle))
  :init
  (setq delete-by-moving-to-trash               t        ; in dired mode
        dired-auto-revert-buffer                t        ; automatically revert buffer
        dired-clean-up-buffers-too              t        ; kill buffers for deleted files
        dired-dwim-target                       t        ; guess target directory
        dired-keep-marker-copy                  nil      ; don't mark copied files
        dired-listing-switches                  "-GAlh --group-directories-first --quoting-style=literal"
        dired-recursive-copies                  'always  ; don't ask me, just do it
        dired-recursive-deletes                 'always  ; ^
        image-dired-show-all-from-dir-max-files 127      ; a bit more
        wdired-allow-to-change-permissions      t)       ; change permissions with Dired

  (with-eval-after-load "dired-aux"
    (add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))
  :config
  (defun my/dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t))))))

;;; UI settings

;; Empty scratch buffer
(setq initial-scratch-message "")

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

;; specify default font
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(when (member "Hack" (font-family-list))
  (add-to-list 'initial-frame-alist '(font . "Hack-10"))
  (add-to-list 'default-frame-alist '(font . "Hack-10")))
;; specify font for all unicode characters
;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; automatic font mapping
;; https://github.com/rolandwalker/unicode-fonts/issues/3
(use-package unicode-fonts
  :config
  (defun my/hook-unicode-fonts-setup (frame)
    "Run unicode-fonts-setup, then remove the hook."
    (progn
      (select-frame frame)
      (unicode-fonts-setup)
      (message "Removing unicode-fonts-setup to after-make-frame-functions hook")
      (remove-hook 'after-make-frame-functions 'my/hook-unicode-fonts-setup)))

  (if initial-window-system
      (unicode-fonts-setup)
    (add-hook 'after-make-frame-functions 'my/hook-unicode-fonts-setup nil)))

(use-package hc-zenburn-theme
  :config
  (custom-set-faces
   '(hl-line ((t (:background "grey29")))))
  (load-theme 'hc-zenburn t))

(use-package fill-column-indicator
  :defer t
  :init
  (setq fci-rule-width 3
        fci-rule-color "grey29"
        fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package popwin
  :init
  (setq popwin:popup-window-height 25)
  :config
  (push "*Async Shell Command*" popwin:special-display-config)
  (push "*Backtrace*" popwin:special-display-config)
  (push '("\*Man " :regexp t) popwin:special-display-config)
  (push "*Warnings*" popwin:special-display-config)
  (popwin-mode 1))

(use-package treemacs
  :defer t
  :bind (:map global-map
              ("C-c v" . treemacs-toggle)
              ("C-c V" . treemacs-projectile))
  :init
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
  :config
  (use-package treemacs-projectile
    :init
    (setq treemacs-header-function #'treemacs-projectile-create-header))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq powerline-height 18)
  :config
  (require 'spaceline-segments)

  (spaceline-define-segment persp-name
    "The current workspace number. Requires `exwm' to be enabled."
    (when (and (fboundp 'exwm-workspace-switch)
               (boundp 'exwm-workspace--list))
      (let* ((num (cl-position (selected-frame) exwm-workspace--list))
             (str (when num (int-to-string num))))
        (when str
          (propertize str 'face 'bold)))))

  (spaceline-define-segment window-number
    "The current window number. Requires `ace-window' to be enabled."
    (when (fboundp 'aw-update)
      (aw-update)
     (let* ((num (read (window-parameter (selected-window) 'ace-window-path)))
             (str (when num (int-to-string num))))
        (when str
          (propertize str 'face 'bold)))))

  (spaceline-emacs-theme))

;;; Keyboard navigation settings

;; Global bindings
(use-package linum
  :commands my/line-number-toggle
  :bind (("C-c l" . hydra-line/body))
  :config
  (defun my/vcs-gutter-mode (state)
    (when (fboundp 'git-gutter-mode)
      (git-gutter-mode state)))

  (defun my/line-number-toggle ()
    (interactive)
    (if (call-interactively 'linum-mode)
        (my/vcs-gutter-mode -1)
      (my/vcs-gutter-mode t)))

  (defhydra hydra-line ()
    "line"
    ("t" my/line-number-toggle "toggle")
    ("j" join-line "join")
    ("q" nil "quit")))

(use-package multiple-cursors
  :bind (("C-c c" . hydra-cursor/body))
  :commands hydra-cursor/body
  :config
  (defhydra hydra-cursor ()
    "cursor"
    ("n" mc/mark-next-like-this "next")
    ("p" mc/mark-previous-like-this "prev")
    ("N" mc/unmark-next-like-this "unmark next")
    ("P" mc/unmark-previous-like-this "unmark prev")
    ("sn" mc/skip-to-next-like-this "skip next")
    ("sp" mc/skip-to-previous-like-this "skip prev")
    ("e" mc/edit-lines "edit lines")
    ("q" nil "quit")))

(use-package god-mode
  :bind (("M-<SPC>" . god-mode-all)
         :map god-local-mode-map
         ("." . repeat)
         ("i" . god-local-mode))
  :init
  (setq god-exempt-major-modes '(magit-popup-mode term-mode)
        god-exempt-predicates '(god-exempt-mode-p))
  (god-mode-all)
  :config
  (defun my/god-update-cursor ()
    (setq cursor-type (if god-local-mode 'box '(bar . 1))))

  (add-hook 'god-mode-enabled-hook #'my/god-update-cursor)
  (add-hook 'god-mode-disabled-hook #'my/god-update-cursor))

;;; Window management settings

;; Undo & redo
(use-package winner
  :demand t
  :init
  (with-eval-after-load "exwm"
    ; undo, redo
    (exwm-input-set-key (kbd "s-u") 'winner-undo)
    (exwm-input-set-key (kbd "s-C-u") 'winner-redo))
  :config
  (winner-mode t))

;; Window management
(use-package windmove
  :commands (windmove-left
             windmove-down
             windmove-up
             windmove-right
             my/windresize-left
             my/windresize-down
             my/windresize-up
             my/windresize-right)
  :init
    (with-eval-after-load "exwm"
      ; move
      (exwm-input-set-key (kbd "s-b") 'windmove-left)
      (exwm-input-set-key (kbd "s-n") 'windmove-down)
      (exwm-input-set-key (kbd "s-p") 'windmove-up)
      (exwm-input-set-key (kbd "s-f") 'windmove-right)
      ; resize
      (exwm-input-set-key (kbd "s-C-b") 'my/windresize-left)
      (exwm-input-set-key (kbd "s-C-n") 'my/windresize-down)
      (exwm-input-set-key (kbd "s-C-p") 'my/windresize-up)
      (exwm-input-set-key (kbd "s-C-f") 'my/windresize-right))
  :config
  ;; Enlarge/Shrink window
  (defun my/windresize-left (delta)
    "Move window splitter DELTA columns left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally delta)
      (enlarge-window-horizontally delta)))

  (defun my/windresize-right (delta)
    "Move window splitter DELTA columns right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally delta)
      (shrink-window-horizontally delta)))

  (defun my/windresize-up (delta)
    "Move window splitter DELTA columns up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window delta)
      (shrink-window delta)))

  (defun my/windresize-down (delta)
    "Move window splitter DELTA columns down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window delta)
      (enlarge-window delta))))

;; Advanced window management
(use-package ace-window
  :commands ace-window
  :init
  (setq aw-dispatch-always t
        aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?s aw-swap-window " Ace - Swap Window")
          (?f aw-flip-window)
          (?v my/window-split-vert " Ace - Split Vert Window")
          (?h my/window-split-horz " Ace - Split Horz Window")
          (?i delete-other-windows " Ace - Maximize Window")
          (?o delete-other-windows)))

  (with-eval-after-load "exwm"
    ; advanced
    (exwm-input-set-key (kbd "s-l") 'ace-window))
  :config
  ;(ace-window-display-mode t)

  (defun my/window-split-vert (window)
    (aw-split-window-vert window)
    (windmove-down))

  (defun my/window-split-horz (window)
    (aw-split-window-horz window)
    (windmove-right)))

(use-package hydra
  :bind (("C-c a" . hydra-window-layout/body))
  :commands hydra-window-layout/body
  :config
  (defhydra hydra-window-layout ()
    "layout"
    ("a" ace-window "ace" :color blue)
    ("P" my/windresize-up)
    ("p" windmove-up "up")
    ("F" my/windresize-right)
    ("f" windmove-right "right")
    ("N" my/windresize-down)
    ("n" windmove-down "down")
    ("B" my/windresize-left)
    ("b" windmove-left "left")
    ("u" winner-undo "undo")
    ("U" winner-redo "redo")
    ("q" nil "quit")))

;;; Editor settings

;; Backups
(setq make-backup-files nil ; stop creating those backup~ files
      auto-save-default nil) ; stop creating those #autosave# files

;; Remove trailing white spaces
(defun my/delete-trailing-whitespace ()
  (let ((mode (with-current-buffer (current-buffer) major-mode)))
       (when (not (or (eq #'markdown-mode mode) (eq #'gfm-mode mode)))
         (delete-trailing-whitespace))))

(add-hook 'before-save-hook #'my/delete-trailing-whitespace)

;; Put a new line at the end of file
(setq require-final-newline t)

;; Scroll line by line
(setq scroll-step           1
      scroll-conservatively 10000)

;; Line wrapping
(defun my/editor-disable-wrapping ()
  (setq truncate-lines t)
  (setq truncate-partial-width-windows nil))

(add-hook 'prog-mode-hook 'my/editor-disable-wrapping)
(add-hook 'term-mode-hook 'my/editor-disable-wrapping)

;; Fix a weird bug with dead keys when Emacs runs in a GUI
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (global-set-key (kbd "<dead-acute> <SPC>")        "'")
            (global-set-key (kbd "<dead-grave> <SPC>")        "`")
            (global-set-key (kbd "<S-dead-tilde> <SPC>")      "~")
            (global-set-key (kbd "<S-dead-diaeresis> <SPC>")  "\"")
            (global-set-key (kbd "<S-dead-circumflex> <SPC>") "^")))

;; Smarter move
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/editor-move-beginning-of-line (arg)
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

;; remap C-a to `my/editor-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/editor-move-beginning-of-line)

;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my/editor-kill-others-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not
               '(lambda (x) (or (buffer-file-name x)
                                (eq 'dired-mode (buffer-local-value 'major-mode x))))
               (buffer-list))))
  (message "Other buffers killed"))

(global-set-key (kbd "C-c b k") #'my/editor-kill-others-buffers)

;; Auto revert buffer on file change
(use-package autorevert
  :diminish auto-revert-mode)

;; Always ALWAYS use UTF-8
(use-package iso-transl
  :ensure nil
  :config
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8))

(defun my/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Use xclip to copy/paste to the terminal from X.
(use-package xclip
  :config
  (xclip-mode t))
;(turn-on-xclip) ;; this function is not call in rxvt

(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face empty lines-tail tabs tab-mark trailing))
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; Auto indent
(setq tab-always-indent 'complete)

(use-package electric
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-indent-mode))

;; Use space instead of tabs.
(defun my/indent-setup-offset ()
  (setq indent-tabs-mode nil
        tab-width 4
        standard-indent tab-width
        c-basic-offset tab-width
        js-indent-level tab-width
        js2-basic-offset tab-width
        sh-basic-offset tab-width
        web-mode-style-padding tab-width
        web-mode-script-padding tab-width
        json-reformat:indent-width tab-width
        tide-format-options '(:indentSize tab-width :tabSize tab-width :convertTabsToSpaces t)
        tab-stop-list (number-sequence tab-width 200 tab-width)))

(add-hook 'conf-mode-hook 'my/indent-setup-offset)
(add-hook 'text-mode-hook 'my/indent-setup-offset)
(add-hook 'prog-mode-hook 'my/indent-setup-offset)

;; http://stackoverflow.com/questions/23692879/emacs24-backtab-is-undefined-how-to-define-this-shortcut-key
(defun my/indent-remove-leading-spaces ()
  "Remove spaces from beginning of line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string tab-width ?\s)))
        (replace-match "")))))

(global-set-key (kbd "<backtab>") 'my/indent-remove-leading-spaces)

;; spell checking
(use-package flyspell
  :bind (( "C-c s". hydra-spellcheck/body))
  :defer t
  :diminish flyspell-mode
  :init
  (setq ispell-list-command "--list"
        ispell-dictionary "american")

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (use-package hydra
    :config
    (require 'ispell)

    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode nil))))

    (let ((langs (list ispell-dictionary "francais")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))

    (defun my/ispell-cycle-languages ()
      "Cycle thought available dictionaries."
      (interactive)
      (let ((lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)))

    (defun my/flyspell-check-next-highlighted-word ()
      "Custom function to spell check next highlighted word."
      (interactive)
      (flyspell-goto-next-error)
      (ispell-word))

    (defhydra hydra-spellcheck (:body-pre (ispell-word))
      "spellcheck"
      ("l" my/ispell-cycle-languages "language")
      ("n" my/flyspell-check-next-highlighted-word "next")
      ("p" flyspell-check-previous-highlighted-word "prev")
      ("q" nil "quit"))))

;;; Source control settings

(use-package git-gutter
  :diminish git-gutter-mode
  :defer t
  :init
  (add-hook 'text-mode-hook 'git-gutter-mode)
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'css-mode-hook  'git-gutter-mode)
  :config
  (custom-set-variables '(git-gutter:separator-sign "|"))
  (set-face-foreground 'git-gutter:separator "grey"))

(use-package magit
  :bind (("C-c g" . hydra-git/body))
  :commands hydra-git/body
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defhydra hydra-git ()
    "git"
    ("n" git-gutter:next-hunk "next hunk")
    ("p" git-gutter:previous-hunk "prev hunk")
    ("g" git-gutter:toggle "gutter")
    ("s" magit-status "status" :color blue)
    ("l" magit-log-buffer-file "file log")
    ("b" magit-blame "blame")
    ("q" nil "quit")))

(use-package ediff
  :defer t
  :init
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)

  (defun command-line-diff (switch)
    "Add a SWITCH to invoke Emacs in diff mode."
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left)))
      (ediff file1 file2)))

  (add-to-list 'command-switch-alist '("diff" . command-line-diff))

  (defun command-line-merge (switch)
    "Add a SWITCH to invoke Emacs in merge mode."
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left))
          (file3 (pop command-line-args-left))
          (file4 (pop command-line-args-left)))
      (ediff-merge-files-with-ancestor file1 file2 file3 nil file4)))

  (add-to-list 'command-switch-alist '("merge" . command-line-merge)))

;;; Coding modes settings

(setq tags-revert-without-query t)

;; Syntax checking
(use-package flycheck
  :diminish "fc"
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; ({[ Pairing
(use-package elec-pair
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Folding
(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Project management
(use-package projectile
  :defer t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init
  (setq projectile-mode-line '(:eval
                               (if (file-remote-p default-directory)
                                   " pt"
                                 (format " pt[%s]" (projectile-project-name)))))

  (add-hook 'conf-mode-hook 'projectile-mode)
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode))

;; Markups modes
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\'"   . web-mode)
         ("\\.tsx\\'"   . web-mode))
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (string= major-mode "web-mode")
                (turn-off-fci-mode)))))

(use-package json-mode
  :bind (:map json-mode-map
              ("C-c C-i" . json-mode-beautify)))

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)

;; Languages modes
(use-package arduino-mode
  :defer t
  :init
  :config
  (use-package company-arduino
    :defer t
    :init
    (eval-after-load 'company '(add-to-list 'company-backends 'company-arduino))))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local
                              flycheck-javascript-eslint-executable
                              (flycheck-locate-config-file-ancestor-directories
                               "node_modules/.bin/eslint" nil)))))

(use-package tern
  :diminish "tr"
  :init
  (eval-after-load 'js2-mode '(add-hook 'js2-mode-hook 'tern-mode))
  (eval-after-load 'web-mode '(add-hook 'web-mode-hook
                                        '(lambda ()
                                           (when (string-equal "jsx" (file-name-extension buffer-file-name))
                                             (tern-mode)))))
  :config
  (use-package company-tern
    :defer t
    :init
    (eval-after-load 'company '(add-to-list 'company-backends 'company-tern))))

(use-package haskell-mode
  :defer t
  :config
  (turn-on-haskell-indent))

(use-package haskell-doc
 :ensure haskell-mode
 :hook (haskell-mode . haskell-doc-mode))

(use-package hindent
 :hook (haskell-mode . hindent-mode)
 :init
 (setq hindent-reformat-buffer-on-save t))

(defvar-local my/intero-disabled nil)

(use-package intero
 :hook (haskell-mode . my/intero-mode)
 :init
 (defun my/intero-mode ()
   (if (not (bound-and-true-p my/intero-disabled))
       (intero-mode))))

(use-package elm-mode
  :defer t
  :init
  (setq elm-format-on-save t
        elm-sort-imports-on-save t
        elm-tags-on-save t
        elm-tags-exclude-elm-stuff nil)
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-elm))
  (use-package flycheck-elm))

(use-package nix-mode
  :defer t)

(use-package csharp-mode
  :defer t)

(use-package omnisharp
  :diminish "o#"
  :bind (:map omnisharp-mode-map
              ("M-." . omnisharp-go-to-definition)
              ("C-c C-t" . omnisharp-current-type-information))
  :init
  (eval-after-load 'csharp-mode '(add-hook 'csharp-mode-hook 'omnisharp-mode))
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

(use-package fsharp-mode
  :defer t)

;;; Completion settings

(use-package company
  :diminish company-mode
  :bind (:map company-mode-map
              ("<tab>" . company-indent-or-complete-common)
         :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-h" . company-quickhelp-manual-begin))
  :init
  (setq company-tooltip-align-annotations t)
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay nil)
    (company-quickhelp-mode 1))

  (defvar-local my/auto-complete-fci-mode-on-p nil)

  (defun my/auto-complete-turn-off-fci (&rest ignore)
    "Safely turn off Fill Column Indicator.
If `fci-mode' is enabled disable it and store its state in special variable.
Argument IGNORE is not used"
    (when (boundp 'fci-mode)
      (setq my/auto-complete-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun my/auto-complete-maybe-turn-on-fci (&rest ignore)
    "Turn on Fill Column Indicator if it was enabled.
If `fci-mode' was enabled turn it on.
Argument IGNORE is not used."
    (when my/auto-complete-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'my/auto-complete-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'my/auto-complete-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'my/auto-complete-maybe-turn-on-fci))

(use-package ivy
  :ensure ivy-hydra
  :ensure swiper
  :ensure counsel
  :ensure smex
  :diminish ivy-mode
  :diminish counsel-mode
  :demand t
  :bind (("C-s"     . swiper)
         ("C-c C-r" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 12

        magit-completing-read-function 'ivy-completing-read
        mu4e-completing-read-function 'ivy-completing-read
        projectile-completion-system 'ivy)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;;; Shell settings

(use-package term
  :bind (:map term-raw-map
         ("M-<SPC>" . god-mode)
         ("C-c a" . hydra-window-layout/body)
         ("M-x" . counsel-M-x)
         :map term-mode-map
         ("M-<SPC>" . god-mode)))

(use-package eshell
  :commands eshell
  :bind (("C-c e" . my/eshell)
         ("C-c E" . my/eshell-new))
  :init
  (setq eshell-banner-message ""
        eshell-cmpl-autolist t
        eshell-cmpl-cycle-completions nil
        eshell-cmpl-cycle-cutoff-length 2
        eshell-cmpl-ignore-case t
        eshell-cp-overwrite-files nil
        eshell-default-target-is-dot t
        eshell-destroy-buffer-when-process-dies t
        eshell-hist-ignoredups t
        eshell-list-files-after-cd t
        eshell-ls-dired-initial-args '("-h")
        eshell-ls-initial-args "-h"
        eshell-review-quick-commands t
        eshell-prompt-function (lambda ()
                                 (concat (if (and (fboundp #'tramp-tramp-file-p)
                                                  (tramp-tramp-file-p default-directory))
                                           (concat
                                            (tramp-file-name-user
                                             (tramp-dissect-file-name default-directory))
                                            "@"
                                            (tramp-file-name-host
                                             (tramp-dissect-file-name default-directory))
                                            " ")
                                           (concat
                                            (user-login-name)
                                            "@"
                                            (system-name)
                                            " "))
                                         (let ((dir (eshell/pwd)))
                                           (if (string= dir (getenv "HOME")) "~"
                                             (let ((dirname (file-name-nondirectory dir)))
                                               (if (string= dirname "") "/" (abbreviate-file-name dir)))))
                                         (if (= (user-uid) 0) " # " " $ ")))
        eshell-save-history-on-exit t
        eshell-stringify nil
        eshell-term-name "ansi"
        eshell-visual-commands '("vi" "top" "htop" "less")
        eshell-history-size 50000
        eshell-output-filter-functions '(eshell-handle-control-codes
                                         eshell-handle-ansi-color
                                         eshell-watch-for-password-prompt))
  :config
  (defun my/eshell-bind-pcomplete ()
    (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point))

  (add-hook 'eshell-mode-hook #'my/eshell-bind-pcomplete)

  (defun my/eshell-is-active (b)
    (eq 'eshell-mode
        (with-current-buffer b major-mode)))

  (defun my/eshell ()
    "Open an instance of eshell."
    (interactive)
    (if (my/eshell-is-active (current-buffer))
        (popwin:close-popup-window)
      (popwin:display-buffer-1
       (save-window-excursion
         (call-interactively #'eshell))
       :default-config-keywords '(:height 25 :stick t))))

  (defun my/eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)))

;;; Org mode settings

(use-package org
  :bind (("C-c o" . hydra-org/body))
  :commands hydra-org/body
  :init
  (setq org-agenda-files (list "~/code/org/agenda")
        org-log-done 'time)
  :config
  (defhydra hydra-org ()
    "org"
    ("a" org-agenda "agenda")
    ("ht" org-shifttab "headlines")
    ("hf" org-metaright "nest")
    ("hb" org-metaleft "un-nest")
    ("hn" org-promote-subtree "nest tree")
    ("hp" org-demote-subtree "un-nest tree")
    ("tn" org-insert-todo-heading "new todo")
    ("q" nil "quit")))

;;; Task manager settings

(use-package prodigy
  :bind (("C-c t" . prodigy))
  :config
  (prodigy-define-tag
    :name 'default
    :kill-process-buffer-on-stop t)

  ;; (prodigy-define-service
  ;;   :name ""
  ;;   :cwd "~/code/"
  ;;   :command ""
  ;;   :args '("" "")
  ;;   :init-async (lambda (done)
  ;;                 (done))
  ;;   :tags '(default))

  ;; Nix Shell
  (prodigy-define-tag
    :name 'nix-shell
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args (prodigy-callback (service)
            `("--pure"
              "--run"
              ,(getf service :arg)))
    :stop-signal 'kill)

  ;; (prodigy-define-service
  ;;   :name ""
  ;;   :cwd "~/code/"
  ;;   :arg ""
  ;;   :port 8080
  ;;   :tags '(default nix-shell))

  ;; SSH Tunnel: https://raw.githubusercontent.com/rejeep/prodigy.el/master/examples/ssh-tunnel.el
  (defun my/prodigy-build-tunnel-args (args)
    "Assemble the ssh tunnel argument list."
    `("-v" ;; allows us to parse for the ready message
      "-N" ;; don't start an interactive shell remotely
      "-C" ;; enable compression
      "-L" ,(concat (cl-getf args :localport) ;; the tunnel spec
                    ":"
                    (cl-getf args :tunnel-host)
                    ":"
                    (cl-getf args :tunnel-port))
      "-l" ,(cl-getf args :user) ;; the user name
      "-p" ,(cl-getf args :port) ;; the remote port
      ,(cl-getf args :host)))    ;; the remote host

  (prodigy-define-tag
    :name 'ssh-tunnel
    :command "ssh"
    :cwd (getenv "HOME")
    :args (prodigy-callback (service)
            (my/prodigy-build-tunnel-args
             (cl-getf service :tunnel)))
    :ready-message "debug1: Entering interactive session.")

  ;; (prodigy-define-service
  ;;   :name "tunnel - 10001::81"
  ;;   :tags '(default ssh-tunnel)
  ;;   :tunnel (list
  ;;            :localport   "10001"
  ;;            :tunnel-host ""
  ;;            :tunnel-port "81"
  ;;            :user        ""
  ;;            :host        ""
  ;;            :port        "22"))

  (defun my/prodigy-build-proxy-args (args)
    "Assemble the ssh proxy argument list."
    `("-v" ;; allows us to parse for the ready message
      "-N" ;; don't start an interactive shell remotely
      "-C" ;; enable compression
      "-D" ,(cl-getf args :localport) ;; the proxy spec
      "-l" ,(cl-getf args :user) ;; the user name
      "-p" ,(cl-getf args :port) ;; the remote port
      ,(cl-getf args :host))) ;; the remote host

  (prodigy-define-tag
    :name 'ssh-proxy
    :command "ssh"
    :cwd (getenv "HOME")
    :args (prodigy-callback (service)
            (my/prodigy-build-proxy-args
             (cl-getf service :proxy)))
    :ready-message "debug1: Entering interactive session.")

  (prodigy-define-service
    :name "proxy - 8888:bibimbap.me"
    :tags '(default ssh-proxy)
    :proxy (list
            :localport "8888"
            :user      (getenv "USER")
            :host      "bibimbap.me"
            :port      "22")))

;;; Mail client settings

(use-package mu4e
  :disabled t
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :bind (:map mu4e-main-mode-map
              ("<f1>" . my/mu4e-account-fastmail)
         :map mu4e-headers-mode-map
              ("<f1>" . my/mu4e-account-fastmail)
              ("d" . my/mu4e-move-to-trash)
         :map mu4e-view-mode-map
              ("d" . my/mu4e-move-to-trash))
  :init
  ;; basic user information
  (setq user-full-name  "Loïc Fontaine")

  ;; a  list of user's e-mail addresses
  (setq mu4e-user-mail-address-list '("loicfontaine@fastmail.fm"
                                      "ljph.fontaine@gmail.com"
                                      "channary.loic@gmail.com"))

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '((:date    . 25)
          (:flags   .  6)
          (:from    . 22)
          (:subject . nil)))

  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300 ;; update every 5 minutes
        mu4e-hide-index-messages t)

  ;; set this to nil so signature is not included by default
  ;; you can include in message with C-c C-w
  (setq mu4e-compose-signature-auto-include 't)

  ;; if you need offline mode, set these -- and create the queue dir
  ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
  (setq smtpmail-queue-mail  nil
        smtpmail-queue-dir  "~/Maildir/queue/cur")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; new mail query
  (setq my/mu4e-new-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed")
        my/mu4e-account-new-mail-query nil)
  :config
  ;; custom bookmarks
  (add-to-list 'mu4e-bookmarks
               '(my/mu4e-account-new-mail-query
                 "New messages"
                 ?n))

  ;; new mail notification
  (use-package mu4e-alert
    :demand t
    :init
    (setq alert-fade-time 10
          mu4e-alert-interesting-mail-query my/mu4e-new-mail-query
          mu4e-alert-email-notification-types '(subjects))
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display))

  ;; display rich-text messages
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; send mail
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; custom move to trash
  ;; https://groups.google.com/forum/#!topic/mu-discuss/m4ORymDlf0E
  (defun my/mu4e-move-to-trash ()
    (interactive)
    (mu4e-mark-set 'move mu4e-trash-folder))

  ;; account setup
  (defun my/mu4e-account-fastmail ()
    (interactive)
    (message "Switching to fastmail account...")

    (setq my/mu4e-account-new-mail-query (concat my/mu4e-new-mail-query
                                                 " AND maildir:/fastmail/*"
                                                 " AND NOT maildir:\"/fastmail/INBOX.Junk Mail\""
                                                 " AND NOT maildir:/fastmail/INBOX.Trash"))

    ;; the next are relative to `mu4e-maildir'
    ;; instead of strings, they can be functions too, see
    ;; their docstring or the chapter 'Dynamic folders'
    (setq mu4e-sent-folder   "/fastmail/INBOX.Sent Items"
          mu4e-drafts-folder "/fastmail/INBOX.Drafts"
          mu4e-trash-folder  "/fastmail/INBOX.Trash")

    ;; the maildirs you use frequently; access them with 'j' ('jump')
    (setq mu4e-maildir-shortcuts
          '(("/fastmail/INBOX"            . ?i)
            ("/fastmail/INBOX.Archive"    . ?a)
            ("/fastmail/INBOX.Sent Items" . ?s)
            ("/fastmail/INBOX.Notes"      . ?n)))

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setq user-mail-address "loicfontaine@fastmail.fm"
          mu4e-compose-signature (concat user-full-name
                                         "\nloicfontaine@fastmail.fm"
                                         "\n"))

    ;; smtp mail setting
    (setq smtpmail-smtp-server "mail.messagingengine.com"
          smtpmail-stream-type 'ssl
          smtpmail-smtp-service 465))

  ;; when you reply to a message, use the identity that the mail was sent to
  ;; -- function that checks to, cc and bcc fields
  (defun my/mu4e-is-message-to (msg rx)
    "Check if to, cc or bcc field in MSG has any address in RX."
    (or (mu4e-message-contact-field-matches msg :to rx)
        (mu4e-message-contact-field-matches msg :cc rx)
        (mu4e-message-contact-field-matches msg :bcc rx)))

  ;; we only do something if we recognize something (i.e. no stupid default)
  (defun my/mu4e-set-from-address ()
    "Set current identity based on to, cc, bcc of original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (if msg
          (cond
           ((my/mu4e-is-message-to msg (list "loicfontaine@fastmail.fm"
                                             "ljph.fontaine@gmail.com"
                                             "channary.loic@gmail.com"))
            (my/mu4e-account-fastmail))))))

  (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-from-address)

  ;; set default account
  (my/mu4e-account-fastmail))

;;; EXWM settings

(use-package exwm
  ;:load-path "~/.emacs.d/site-lisp/exwm/"
  :disabled t
  :commands exwm-enable
  :bind (("C-c b"   . exwm-workspace-switch-to-buffer)
         ("C-c s"   . exwm-workspace-swap)
         ("C-c C-m" . exwm-workspace-move-window)
         :map exwm-mode-map
         ("C-c b" . exwm-workspace-switch-to-buffer)
         ("C-c s" . exwm-workspace-swap)
         ("C-q"   . exwm-input-send-next-key))
  :init
  ;; (setq debug-on-error t
  ;;       debug-on-quit t
  ;;       edebug-all-forms t
  ;;       exwm-debug-on t)

  (setq exwm-workspace-number 6)
  :config
  ;(use-package xelb :load-path "~/.emacs.d/site-lisp/xelb/")

  ;; Rename buffer based on its title / class
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (if (s-contains? exwm-class-name exwm-title t)
                  (exwm-workspace-rename-buffer exwm-title)
                (exwm-workspace-rename-buffer (concat exwm-title " - " exwm-class-name)))))

  ;; Local key binding prefix for line mode
  (push ?\C-q exwm-input-prefix-keys)

  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             (when (and exwm-class-name
  ;;                        (string= exwm-class-name "URxvt"))
  ;;               (setq-local exwm-input-prefix-keys '()))))

  ;; 's-q': Reset
  (exwm-input-set-key (kbd "s-q") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda () (interactive) (exwm-workspace-switch ,i))))

  ;; 's-r': Launch application
  (exwm-input-set-key (kbd "s-r")
                      (lambda (command)
                        (interactive (list (read-shell-command "Run: ")))
                        (start-process-shell-command command nil command)))

  ;; 'M-<tab>: Switch buffer
  (defun my/exwm-buffer-switch ()
    (interactive)
    (let* ((current-buffers (cl-remove-if-not (lambda (b)
                                                (with-current-buffer b
                                                  (eq exwm--frame exwm-workspace--current)))
                                              (mapcar (lambda (pair)
                                                        (cdr pair))
                                                      exwm--id-buffer-alist)))
           (current-buffer-position (cl-position (current-buffer) current-buffers))
           (next-buffer (if (or (not current-buffer-position)
                                (= current-buffer-position (- (length current-buffers) 1)))
                            (nth 0 current-buffers)
                          (nth (+ current-buffer-position 1) current-buffers))))
      (switch-to-buffer next-buffer nil t)))

  (exwm-input-set-key (kbd "M-<tab>") #'my/exwm-buffer-switch)

  ;; Global apps shortcuts
  (defun my/exwm-global-command (key command)
    (exwm-input-set-key (kbd key) (lambda ()
                                    (interactive)
                                    (start-process-shell-command command nil command))))

  (my/exwm-global-command "<XF86AudioRaiseVolume>" "audio-volume.sh up")
  (my/exwm-global-command "<XF86AudioLowerVolume>" "audio-volume.sh down")
  (my/exwm-global-command "<XF86AudioMute>" "audio-volume.sh toggle")
  (my/exwm-global-command "s-M-t" (getenv "TERMINAL"))
  (my/exwm-global-command "s-M-w" (getenv "BROWSER"))
  (my/exwm-global-command "s-M-e" (getenv "VISUAL"))
  (my/exwm-global-command "s-M-l" "slock")
  (my/exwm-global-command "s-M-f" "pcmanfm")

  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ([?\M-w] . (?\C-c))
     ([?\C-w] . (?\C-x))
     ([?\C-y] . (?\C-v))))

  ;; System tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Services
  (add-hook 'exwm-init-hook
            (lambda ()
              (start-process-shell-command "dex" nil "dex -ae EXWM")))

  ;; Multi monitor support
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-2" 1 "HDMI-2" 2 "HDMI-2"
                                            3 "eDP-1" 4 "eDP-1" 5 "eDP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --primary --output HDMI-2 --left-of eDP-1 --auto")))
  (exwm-randr-enable)

  ;; Date & time
  (use-package minibuffer-line
    :init
    (setq minibuffer-line-refresh-interval 20
          minibuffer-line-format '((:eval
                                    (format-time-string "%a %Y/%m/%d %I:%M %p"))))
    :config
    (custom-set-faces
     '(minibuffer-line ((t (:height 1.0
                            :weight regular
                            :foreground "LightBlue3")))))
    (minibuffer-line-mode)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:separator-sign "|")
 '(package-selected-packages
   (quote
    (tidal go-mode protobuf-mode god-mode god org-mode docker-compose-mode arduino-mode company-arduino dockerfile-mode treemacs-projectile treemacs hindent intero-mode intero flycheck-elm toml-mode markdown-mode+ yaml-mode swiper ivy fsharp-mode omnisharp csharp-mode elm-mode typescript-mode tern flycheck company mu4e-alert rbenv nvm xclip web-mode use-package tide spaceline smex rainbow-delimiters purescript-mode psc-ide projectile prodigy popwin nix-mode multiple-cursors modalka minibuffer-line magit load-dir json-mode js2-mode ivy-hydra hc-zenburn-theme haskell-mode git-gutter fill-column-indicator exwm elfeed counsel company-tern company-quickhelp ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey29"))))
 '(minibuffer-line ((t (:height 1.0 :weight regular :foreground "LightBlue3")))))
