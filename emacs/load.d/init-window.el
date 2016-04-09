;;; init-window.el --- Window settings
;;; Commentary:
;;; Code:

;; Undo & redo
(use-package winner
  :demand t
  :bind (("M-C-p"   . winner-undo)
         ("M-C-S-p" . winner-redo))
  :config
  (winner-mode t))

;; Window management
(use-package windmove
  :ensure t
  :bind (; move
         ("M-C-h" . windmove-left)
         ("M-C-j" . windmove-down)
         ("M-C-k" . windmove-up)
         ("M-C-l" . windmove-right)
         ; resize
         ("M-C-S-h" . my/windresize-left)
         ("M-C-S-j" . my/windresize-down)
         ("M-C-S-k" . my/windresize-up)
         ("M-C-S-l" . my/windresize-right))
  :init
  (global-unset-key (kbd "M-C-h"))
  :config
  ;; Enlarge/Shrink window
  (defun my/windresize-left (delta)
    "Move window splitter DELTA columns left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally delta)
      (enlarge-window-horizontally delta)))

  (defun my/windresize-right (delta)
    "Move window splitter DELTA columns right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally delta)
      (shrink-window-horizontally delta)))

  (defun my/windresize-up (delta)
    "Move window splitter DELTA columns up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window delta)
      (shrink-window delta)))

  (defun my/windresize-down (delta)
    "Move window splitter DELTA columns down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window delta)
      (enlarge-window delta))))

;; Advanced window management
(use-package ace-window
  :ensure t
  :bind (("M-p" . ace-window))
  :init
  (setq aw-dispatch-always t
        aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?s aw-swap-window " Ace - Swap Window")
          (?f aw-flip-window)
          (?v my/window-split-vert " Ace - Split Vert Window")
          (?h my/window-split-horz " Ace - Split Horz Window")
          (?i delete-other-windows " Ace - Maximize Window")
          (?o delete-other-windows)))
  :config
  ;(ace-window-display-mode t)

  (defun my/window-split-vert (window)
    (aw-split-window-vert window)
    (windmove-down))

  (defun my/window-split-horz (window)
    (aw-split-window-horz window)
    (windmove-right)))

(with-eval-after-load "init-exwm"
  (add-hook 'my/exwm-config-hook
            (lambda ()
              ; undo, redo
              (exwm-input-set-key (kbd "s-u") 'winner-undo)
              (exwm-input-set-key (kbd "s-C-u") 'winner-redo)
              ; move
              (exwm-input-set-key (kbd "s-h") 'windmove-left)
              (exwm-input-set-key (kbd "s-j") 'windmove-down)
              (exwm-input-set-key (kbd "s-k") 'windmove-up)
              (exwm-input-set-key (kbd "s-l") 'windmove-right)
              ; resize
              (exwm-input-set-key (kbd "s-C-h") 'my/windresize-left)
              (exwm-input-set-key (kbd "s-C-j") 'my/windresize-down)
              (exwm-input-set-key (kbd "s-C-k") 'my/windresize-up)
              (exwm-input-set-key (kbd "s-C-l") 'my/windresize-right)
              ; advanced
              (exwm-input-set-key (kbd "s-C-p") 'ace-window))))

(provide 'init-window)

;;; init-window.el ends here
