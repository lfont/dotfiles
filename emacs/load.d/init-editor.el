;;; init-editor.el --- editor settings
;;; Commentary:
;;; Code:

;; Backups
(setq make-backup-files nil ; stop creating those backup~ files
      auto-save-default nil) ; stop creating those #autosave# files

;; Remove trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Put a new line at the end of file
(setq require-final-newline t)

;; Scroll line by line
(setq scroll-step           1
      scroll-conservatively 10000)

;; Line wrapping
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Leader key
;; http://emacs.stackexchange.com/questions/12961/how-can-i-globally-replace-c-c-with-another-key-binding
;(define-key input-decode-map (kbd "<M-SPC>") (kbd "<leader>"))
(define-key key-translation-map (kbd "<M-SPC>") (kbd "C-c"))
(global-set-key (kbd "C-c M-s") 'save-buffer)
(global-set-key (kbd "C-c M-c") 'save-buffers-kill-terminal)

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
(defun my/move-beginning-of-line (arg)
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

;; remap C-a to `my/move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/move-beginning-of-line)

;; Auto revert buffer on file change
(use-package autorevert
  :diminish auto-revert-mode)

;; Always ALWAYS use UTF-8
(use-package iso-transl
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

;; Use xclip to copy/paste to the terminal from X.
(use-package xclip
  :ensure t
  :config
  (xclip-mode t))
;(turn-on-xclip) ;; this function is not call in rxvt

(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face empty lines-tail tabs tab-mark trailing))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(provide 'init-editor)

;;; init-editor.el ends here
