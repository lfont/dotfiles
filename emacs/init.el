;; https://github.com/mklappstuhl/dotfiles/blob/master/emacs.d/init.el
;; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
;; list the packages you want
(setq package-list '(
                     flx-ido
                     ido-vertical-mode
                     ;smex

                     helm
                     projectile
                     helm-projectile

                     magit
                     git-gutter

                     auto-complete
                     tern
                     tern-auto-complete

                     flycheck
                     php-mode
                     less-css-mode

                     zenburn-theme
                     molokai-theme
                     monokai-theme

                     fill-column-indicator
                     multiple-cursors
                     highlight-chars
                     rainbow-delimiters

                     xclip

                     bbdb
                     gnus-desktop-notify
                     w3m

                     jabber))

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
;(setq linum-format "%d ")

(global-hl-line-mode t)
(column-number-mode 1)

;; Tab behavior
(add-hook 'prog-mode-hook (lambda ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq tab-stop-list (number-sequence 4 200 4))
    ;(setq indent-line-function 'insert-tab)
))

(setq inhibit-startup-message t)
(setq visible-bell 'top-bottom)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(load-theme 'molokai t)
(set-frame-font "Inconsolata 11")

(add-hook 'prog-mode-hook (lambda ()
    ;(electric-indent-mode)
    (electric-pair-mode)
    (flycheck-mode)
    (flyspell-prog-mode)))

(dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

(setq ispell-program-name "/usr/bin/aspell"
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

(global-set-key (kbd "<f5>")     'ispell-word)
(global-set-key (kbd "M-<f5>")   'ispell-cycle-languages)
(global-set-key (kbd "C-S-<f5>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "C-<f5>")   'flyspell-check-next-highlighted-word)

;; Window Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))

(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
;(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(setq
  ;helm-google-suggest-use-curl-p t
  ;helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
  ;helm-quick-update t ; do not display invisible candidates
  ;helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
  ;helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
  ;helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

  helm-split-window-default-side 'other ;; open helm buffer in another window
  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
  ;helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                      ;'(picture-mode artist-mode))
  ;helm-candidate-number-limit 200 ; limit the number of displayed canidates
  ;helm-M-x-requires-pattern 0     ; show all candidates when set to 0
  ;helm-boring-file-regexp-list
  ;'("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
  ;helm-ff-file-name-history-use-recentf t
  ;helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                      ; when reaching top or bottom of source.
  ;ido-use-virtual-buffers t      ; Needed in helm-buffers-list
  ;helm-buffers-fuzzy-matching t  ; fuzzy matching buffer names when non--nil
                                 ; useful in helm-mini that lists buffers
 )

;; Save current position to mark ring when jumping to a different place
;(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'flx-ido)
;(ido-mode 1)
;(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching -1)

(require 'helm-projectile)

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
     (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)))

(require 'auto-complete)
(global-auto-complete-mode t)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(require 'git-gutter)
(global-git-gutter-mode t)
(when (display-graphic-p) (git-gutter:linum-setup))
(unless (display-graphic-p)
    (custom-set-variables '(git-gutter:separator-sign "|"))
    (set-face-foreground 'git-gutter:separator "grey"))

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

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook
    (lambda ()
        (hc-highlight-tabs)
        (hc-highlight-trailing-whitespace)))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'jabber)
(setq jabber-account-list '(("lfontaine@mappyim"
                              (:network-server . "mappyim")
                              (:port . 5223)
                              (:connection-type . ssl))))

(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)

(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))

(load-library "notify")
(require 'notify)

(defun notify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify (format "%s" (jabber-jid-displayname from))
             text)))

(add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

;; Use xclip to copy/paste to the terminal from X.
(xclip-mode 1)

;; Enable semantic mode globally
(semantic-mode 1)

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
