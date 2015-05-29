;;; init.el --- user settings
;;; Commentary:
;;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el

;;; Code:
(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Packages settings
(require 'package)
(let ((package-list '(flx-ido
                      ido-vertical-mode

                      helm
                      projectile
                      helm-projectile

                      magit
                      git-gutter

                      auto-complete
                      tern
                      tern-auto-complete

                      flycheck

                      js2-mode
                      less-css-mode
                      haskell-mode
                      php-mode

                      molokai-theme

                      fill-column-indicator
                      multiple-cursors
                      rainbow-delimiters

                      xclip

                      multi-term
                      buffer-move

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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(unless (display-graphic-p)
  (menu-bar-mode -1))

(load-theme 'molokai t)
(add-to-list 'default-frame-alist
             '(font . "Terminus-13:Regular"))

;; Scroll line by line
(setq scroll-step           1
      scroll-conservatively 10000)

;;; Windows related operations
;; Undo & redo
(when (fboundp 'winner-mode)
      (winner-mode t))

;; Split & Resize
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C--") 'shrink-window)
(global-set-key (kbd "C-+") 'enlarge-window)

;; Navgating: Windmove uses C-<up> etc.
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; Swap buffers: M-<up> etc.
(require 'buffer-move)
(global-set-key (kbd "M-<up>")    'buf-move-up)
(global-set-key (kbd "M-<down>")  'buf-move-down)
(global-set-key (kbd "M-<right>") 'buf-move-right)
(global-set-key (kbd "M-<left>")  'buf-move-left)

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
  "Use space instead of tabs."
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 4 200 4)))

(add-hook 'text-mode-hook 'set-custom-tab-behavior)
(add-hook 'prog-mode-hook 'set-custom-tab-behavior)
(add-hook 'css-mode-hook  'set-custom-tab-behavior)

;; http://stackoverflow.com/questions/23692879/emacs24-backtab-is-undefined-how-to-define-this-shortcut-key
(defun un-indent-by-removing-spaces (count)
  "Remove COUNT spaces from beginning of line."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string count ?\s)))
        (replace-match "")))))

(defun un-indent-by-removing-4-spaces ()
  "Remove 4 spaces from beginning of line."
  (interactive)
  (un-indent-by-removing-spaces 4))

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not
               '(lambda (x) (or (buffer-file-name x)
                                (eq 'dired-mode (buffer-local-value 'major-mode x))))
               (buffer-list))))
  (message "Other buffers killed"))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

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
;(turn-on-xclip) ;; this function is not call in rxvt

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

;; Fix a weird bug with dead keys when Emacs runs in a GUI
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (global-set-key (kbd "<dead-acute> <SPC>")        "'")
            (global-set-key (kbd "<dead-grave> <SPC>")        "`")
            (global-set-key (kbd "<S-dead-tilde> <SPC>")      "~")
            (global-set-key (kbd "<S-dead-diaeresis> <SPC>")  "\"")
            (global-set-key (kbd "<S-dead-circumflex> <SPC>") "^")))

;; Spell checking
(require 'ispell)
(setq ispell-program-name "aspell"
      ispell-list-command "--list")

(let ((langs '("american" "francais")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun ispell-cycle-languages ()
  "Cycle thought available dictionaries."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode nil))))

(global-set-key (kbd "<f7>")     'ispell-word)
(global-set-key (kbd "M-<f7>")   'ispell-cycle-languages)
(global-set-key (kbd "C-S-<f7>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "C-<f7>")   'flyspell-check-next-highlighted-word)

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

(add-to-list 'command-switch-alist '("merge" . command-line-merge))

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
(global-set-key (kbd "C-c C-l")    'mc/edit-lines)
(global-set-key (kbd "C-c C-d")    'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-k")    'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-u")    'mc/unmark-next-like-this)

(require 'whitespace)
(setq whitespace-style '(face empty lines-tail tabs tab-mark trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(when (require 'projectile nil t)
  (projectile-global-mode))

(when (and (require 'helm         nil t)
           (require 'helm-config  nil t)
           (require 'helm-command nil t)
           (require 'helm-files   nil t))
  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-in-side-p nil
   ;; Fuzzy matching
   helm-M-x-fuzzy-match        t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match    t)

  (helm-mode t)
  (helm-autoresize-mode t)

  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (global-set-key (kbd "C-x b")   'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (when (require 'helm-projectile nil t)
    (defun helm-projectile-switch-buffer ()
      "Use Helm instead of ido to switch buffer in projectile."
      (interactive)
      (helm :sources helm-source-projectile-buffers-list
            :buffer "*helm projectile buffers*"
            :prompt (projectile-prepend-project-name "Switch to buffer: ")))

    ;; Override some projectile keymaps
    (eval-after-load 'projectile
      '(progn
         (define-key projectile-command-map (kbd "b") 'helm-projectile-switch-buffer)
         (define-key projectile-command-map (kbd "f") 'helm-projectile)
         (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)))))

;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)

  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/bin/zsh"))

(when (require 'term nil t)
  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
              (cons "C-r"     'term-send-reverse-search-history)))

  (defconst term-function-key-alist '((f1 . "\e[OP")
                                      (f2 . "\e[OQ")
                                      (f3 . "\e[OR")
                                      (f4 . "\e[OS")))

  (defun term-send-function-key ()
    (interactive)
    (let* ((char last-input-event)
           (output (cdr (assoc char term-function-key-alist))))
      (term-send-raw-string output)))

  (dolist (spec term-function-key-alist)
    (define-key term-raw-map
      (read-kbd-macro (format "<%s>" (car spec)))
      'term-send-function-key))

  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument.
      (let* ((start (match-beginning 0))
             (command-code (aref message (+ start 6)))
             (argument
              (save-match-data
                (substring message
                           (+ start 8)
                           (string-match "\r?\n" message
                                         (+ start 8))))))
        ;; Delete this command from MESSAGE.
        (setq message (replace-match "" t t message))

        (cond ((= command-code ?c)
               (setq term-ansi-at-dir argument))
              ((= command-code ?h)
               (setq term-ansi-at-host argument))
              ((= command-code ?u)
               (setq term-ansi-at-user argument))
              ((= command-code ?e)
               (save-excursion
                 (find-file-other-window argument)))
              ((= command-code ?x)
               (save-excursion
                 (find-file argument))))))

    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
            (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
        (setq default-directory (if (string= term-ansi-at-host (system-name))
                                    (concatenate 'string term-ansi-at-dir "/")
                                  (format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message))

;; autoload optional files
(autoload 'notify "notify")
(autoload 'offlineimap-get-password "offlineimap")
(autoload 'my/jabber-start "init-jabber")
(autoload 'my/mu4e-start "init-mu4e")

;;; init.el ends here
