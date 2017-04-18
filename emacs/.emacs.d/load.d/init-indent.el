;;; init-indent.el -- Indentation setup.
;;; Commentary:
;;; Code:

;; Auto indent
(use-package electric
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-indent-mode))

;; Use space instead of tabs.
(defun my/indent-setup-offset ()
  (setq indent-tabs-mode nil
        tab-width 2
        standard-indent tab-width
        c-basic-offset tab-width
        js-indent-level tab-width
        js2-basic-offset tab-width
        sh-basic-offset tab-width
        web-mode-style-padding tab-width
        web-mode-script-padding tab-width
        json-reformat:indent-width tab-width
        tide-format-options '(:indentSize 2 :tabSize 2 :convertTabsToSpaces t)
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

(provide 'init-indent)

;;; init-indent.el ends here
