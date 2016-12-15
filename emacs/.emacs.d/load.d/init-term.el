;;; init-term.el --- term setup
;;; Commentary:
;;; Code:

;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
(use-package term
  :disabled t
  :load-path "~/.emacs.d/site-lisp/"
  :bind (("<f5>" . my/multi-term))
  :config
  (setq term-buffer-maximum-size 0)

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
            (format "%s@%s:%s"
                    term-ansi-at-user
                    term-ansi-at-host
                    term-ansi-at-dir))
      (set-buffer-modified-p nil)
      (setq default-directory (if (string= term-ansi-at-host (system-name))
                                  (concatenate 'string term-ansi-at-dir "/")
                                (format "/%s@%s:%s/"
                                        term-ansi-at-user
                                        term-ansi-at-host
                                        term-ansi-at-dir))))
    message)

  ;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/
  (use-package multi-term :ensure t)
  (use-package popwin     :ensure t)

  (setq multi-term-buffer-name "term")

  ;; Keys captured by Emacs
  (add-to-list 'term-unbind-key-list "C-g")

  ;; Keys captured by term
  (add-to-list 'term-bind-key-alist '("C-c n"   . multi-term))
  (add-to-list 'term-bind-key-alist '("C-c r"   . my/multi-term-rename-buffer))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-y"     . my/term-paste))
  (add-to-list 'term-bind-key-alist '("M-["     . multi-term-prev))
  (add-to-list 'term-bind-key-alist '("M-]"     . multi-term-next))

  (defun my/multi-term-rename-buffer (new-name)
    (interactive "MRename buffer (to new name): ")
    (rename-buffer (format "*%s<%s>*"
                           multi-term-buffer-name
                           new-name)))

  (defun my/multi-term-is-term (b)
    (eq 'term-mode
        (with-current-buffer b major-mode)))

  (defun my/multi-term-get-terms (buffers)
    (cl-remove-if (lambda (b)
                    (not
                     (my/multi-term-is-term b)))
                  buffers))

  (defun my/multi-term ()
    (interactive)
    (if (my/multi-term-is-term (current-buffer))
        (popwin:close-popup-window)
      (popwin:display-buffer-1
       (let ((terms (my/multi-term-get-terms (buffer-list))))
         (if terms
             (car terms)
           (save-window-excursion
             (call-interactively 'multi-term))))
       :default-config-keywords '(:height 25 :stick t))))

  (defun my/term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string string (current-kill 0))))

  ;; http://stackoverflow.com/questions/2396680/let-emacs-send-fn-keys-to-programs-in-ansi-term
  (defconst my/term-function-key-alist '((f1  . "\eOP")
                                         (f2  . "\eOQ")
                                         (f3  . "\eOR")
                                         (f4  . "\eOS")
                                         (f5  . "\e[15~")
                                         (f6  . "\e[17~")
                                         (f7  . "\e[18~")
                                         (f8  . "\e[19~")
                                         (f9  . "\e[20~")
                                         (f10 . "\e[21~")
                                         (f11 . "\e[23~")
                                         (f12 . "\e[24~")))

  (defun my/term-send-function-key ()
    (interactive)
    (let* ((char last-input-event)
           (output (cdr (assoc char my/term-function-key-alist))))
      (term-send-raw-string output)))

  (dolist (spec my/term-function-key-alist)
    (define-key term-raw-map
      (read-kbd-macro (format "<%s>" (car spec)))
      'my/term-send-function-key)))

(provide 'init-term)

;;; init-term.el ends here
