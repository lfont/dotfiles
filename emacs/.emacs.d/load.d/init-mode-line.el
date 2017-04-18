;;; init-mode-line.el --- mode line settings
;;; Commentary:
;;; Code:

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq powerline-height 18)
  :config
  (require 'spaceline-segments)

  (spaceline-define-segment persp-name
    "The current workspace number. Requires `exwm' to be enabled."
    (when (and (fboundp 'exwm-workspace-switch)
               (boundp 'exwm-workspace--list))
      (let* ((num (cl-position (selected-frame) exwm-workspace--list))
             (str (when num (int-to-string num))))
        (when str
          (propertize str 'face 'bold)))))

  (spaceline-define-segment window-number
    "The current window number. Requires `ace-window' to be enabled."
    (when (fboundp 'aw-update)
      (aw-update)
      (let* ((num (read (window-parameter (selected-window) 'ace-window-path)))
             (str (when num (int-to-string num))))
        (when str
          (propertize str 'face 'bold)))))

  (spaceline-emacs-theme)
  (with-eval-after-load "helm" (spaceline-helm-mode)))

(provide 'init-mode-line)

;;; init-mode-line.el ends here
