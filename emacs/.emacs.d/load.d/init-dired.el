;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:

(use-package dired
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

(provide 'init-dired)

;;; init-dired.el ends here
