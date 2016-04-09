;;; init-auto-complete.el --- Code auto completion
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-mode-map
              ("M-<tab>" . company-complete))
  :init
  (setq company-tooltip-align-annotations t)
  (global-company-mode 1)
  :config
  (use-package company-quickhelp
    :ensure t
    :init
    (setq company-quickhelp-delay nil)
    :config
    (company-quickhelp-mode 1))

  (use-package tern
    :ensure t
    :diminish tern-mode
    :defer t
    :init
    (add-hook 'js-mode-hook 'tern-mode)
    :config
    (use-package company-tern
      :ensure t
      :defer t
      :init
      (add-to-list 'company-backends 'company-tern))))

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
