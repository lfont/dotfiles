;;; init-term.el --- term setup
;;; Commentary:
;;; Code:

;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
(use-package term
  :bind (("<f5>" . my/multi-term))
  :config
  (setq term-buffer-maximum-size 10000)

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
  (use-package f          :ensure t)

  (setq multi-term-buffer-name "term")

  (add-to-list 'term-unbind-key-list "C-g")

  (defun my/multi-term-rename-buffer (new-name)
    (interactive "MRename buffer (to new name): ")
    (rename-buffer (format "*%s<%s>*"
                           multi-term-buffer-name
                           new-name)))

  (add-to-list 'term-bind-key-alist '("C-c r" . my/multi-term-rename-buffer))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
  (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))

  (defun my/multi-term-is-term (b)
    (eq 'term-mode
        (with-current-buffer b major-mode)))

  (defun my/multi-term-get-terms (buffers)
    (cl-remove-if (lambda (b)
                    (not
                     (my/multi-term-is-term b)))
                  buffers))

  (defun my/multi-term-get-term (cwd terms)
    (car (cl-remove-if (lambda (b)
                         (not
                          (string-equal cwd
                                        (let ((name (buffer-file-name b)))
                                          (string-match ":\\(.*\\)" name)
                                          (expand-file-name (match-string 1 name))))))
                       terms)))

  (defun my/multi-term ()
    (interactive)
    (if (my/multi-term-is-term (current-buffer))
        (popwin:close-popup-window)
      (progn
        (popwin:display-buffer-1
         (let* ((cwd (f-dirname (or buffer-file-name "~/nil")))
                (term (my/multi-term-get-term cwd (my/multi-term-get-terms (buffer-list)))))
           (or term
               (save-window-excursion
                 (call-interactively 'multi-term))))
         :default-config-keywords '(:height 25 :stick t))))))

(provide 'init-term)

;;; init-term.el ends here
