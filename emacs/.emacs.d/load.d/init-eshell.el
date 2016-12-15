;;; init-eshell.el --- elisp shell
;;; Commentary:
;;; Code:

(use-package eshell
  :commands eshell
  :bind (("<f5>" . my/eshell))
  :config
  (use-package popwin :ensure t)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>") 'my/eshell-pcomplete)))

  (defun my/eshell-pcomplete ()
    (interactive)
    (pcomplete-std-complete))

  (defun my/eshell-is-shell (b)
    (eq 'eshell-mode
        (with-current-buffer b major-mode)))

  (defun my/eshell ()
    (interactive)
    (if (my/eshell-is-shell (current-buffer))
        (popwin:close-popup-window)
      (popwin:display-buffer-1
       (save-window-excursion
         (call-interactively 'eshell))
       :default-config-keywords '(:height 25 :stick t)))))
