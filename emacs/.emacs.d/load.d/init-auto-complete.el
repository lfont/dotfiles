;;; init-auto-complete.el --- Code auto completion
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-mode-map
              ("<tab>" . my/auto-complete-or-indent))
  :init
  (setq company-tooltip-align-annotations t)
  (add-hook 'conf-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (defun my/auto-complete-or-indent ()
    (interactive)
    (if (and (not (bolp))
             (not (looking-back "[[:space:]]+" (line-beginning-position)))
             (company-manual-begin))
        (company-complete-common)
      (indent-according-to-mode)))

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
  (add-hook 'company-completion-cancelled-hook 'my/auto-complete-maybe-turn-on-fci)

  (use-package company-quickhelp
    :ensure t
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :init
    (setq company-quickhelp-delay nil)
    (company-quickhelp-mode 1)))

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
