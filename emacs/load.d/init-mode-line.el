;;; init-mode-line.el --- mode line settings
;;; Commentary:
;;; Code:

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq powerline-height 18
        powerline-default-separator 'wave
        spaceline-window-numbers-unicode nil
        spaceline-workspace-numbers-unicode nil)
  :config
  (require 'spaceline-segments)

  (spaceline-define-segment window-number
    "The current window number. Requires `ace-window' to be enabled."
    (when (fboundp 'aw-update)
      (aw-update)
      (let* ((num (read (window-parameter (selected-window) 'ace-window-path)))
             (str (when num (int-to-string num))))
        (if spaceline-window-numbers-unicode
            (spaceline--unicode-number str)
          (propertize str 'face 'bold)))))

  (spaceline-define-segment workspace-number
    "The current workspace number. Requires `exwm' to be enabled."
    (when (fboundp 'exwm-workspace-switch)
      (let* ((num (cl-position (selected-frame) exwm-workspace--list))
             (str (when num (int-to-string num))))
        (when str
          (if spaceline-workspace-numbers-unicode
              (spaceline--unicode-number str)
            (propertize str 'face 'bold))))))

  (spaceline-emacs-theme)
  (with-eval-after-load "helm" (spaceline-helm-mode)))

(provide 'init-mode-line)

;;; init-mode-line.el ends here
