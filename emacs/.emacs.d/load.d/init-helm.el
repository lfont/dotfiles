;;; init-helm.el --- Helm settings
;;; Commentary:
;;; Code:

(use-package helm
  :ensure t
  :diminish helm-mode
  :demand t
  :disabled t
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ;; rebind tab to run persistent action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB works in terminal
         ("C-i"   . helm-execute-persistent-action)
         ;; list actions using C-z
         ("C-z"   . helm-select-action)
         ;; Override some projectile keymaps
         :map projectile-command-map
         ("b" . helm-projectile-switch-buffer)
         ("d" . helm-projectile-find-dir)
         ("f" . helm-projectile)
         ("p" . helm-projectile-switch-project))
  :init
  (require 'helm-config)

  (setq
   ;; split current window and display to bottom
   helm-split-window-in-side-p t
   helm-split-window-default-side 'below
   ;; Fuzzy matching
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t)
  :config
  (ido-mode -1)
  (helm-mode 1)

  (use-package helm-projectile
    :ensure t
    :config
    (defun helm-projectile-switch-buffer ()
      "Use Helm instead of ido to switch buffer in projectile."
      (interactive)
      (helm :sources helm-source-projectile-buffers-list
            :buffer "*helm projectile buffers*"
            :prompt (projectile-prepend-project-name "Switch to buffer: ")))))

(provide 'init-helm)

;;; init-helm.el ends here
