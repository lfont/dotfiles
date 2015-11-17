(require 'helm-config)

(setq
   ;; always split to bottom
   ;helm-always-two-windows t
   ;; split current window and display to bottom
   helm-split-window-in-side-p t
   helm-split-window-default-side 'below
   ;; Fuzzy matching
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t)

(helm-mode 1)
;(helm-autoresize-mode t)

(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
(define-key helm-map (kbd "C-z") 'helm-select-action)

(when (require 'helm-projectile nil t)
  (defun helm-projectile-switch-buffer ()
    "Use Helm instead of ido to switch buffer in projectile."
    (interactive)
    (helm :sources helm-source-projectile-buffers-list
          :buffer "*helm projectile buffers*"
          :prompt (projectile-prepend-project-name "Switch to buffer: ")))

  ;; Override some projectile keymaps
  (eval-after-load 'projectile
    '(progn
       (define-key projectile-command-map (kbd "b") 'helm-projectile-switch-buffer)
       (define-key projectile-command-map (kbd "f") 'helm-projectile)
       (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project))))
