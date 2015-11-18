;; Auto indent
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Tab behavior
(defun my/indent-use-spaces ()
  "Use space instead of tabs."
  (setq indent-tabs-mode nil
        tab-width 2
        standard-indent tab-width
        c-basic-offset tab-width
        js2-basic-offset tab-width
        tab-stop-list (number-sequence tab-width 200 tab-width)))

(add-hook 'text-mode-hook 'my/indent-use-spaces)
(add-hook 'prog-mode-hook 'my/indent-use-spaces)
(add-hook 'css-mode-hook  'my/indent-use-spaces)

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
