;;; init-window.el --- Window settings
;;; Commentary:
;;; Code:

;; Undo & redo
(use-package winner
  :demand t
  :init
  (with-eval-after-load "init-exwm"
    (add-hook 'my/exwm-config-hook
              (lambda ()
                ; undo, redo
                (exwm-input-set-key (kbd "s-u") 'winner-undo)
                (exwm-input-set-key (kbd "s-C-u") 'winner-redo))))
  :config
  (winner-mode t))

;; Window management
(use-package windmove
  :ensure t
  :commands (windmove-left
             windmove-down
             windmove-up
             windmove-right
             my/windresize-left
             my/windresize-down
             my/windresize-up
             my/windresize-right)
  :init
  (with-eval-after-load "init-exwm"
    (add-hook 'my/exwm-config-hook
              (lambda ()
                ; move
                (exwm-input-set-key (kbd "s-b") 'windmove-left)
                (exwm-input-set-key (kbd "s-n") 'windmove-down)
                (exwm-input-set-key (kbd "s-p") 'windmove-up)
                (exwm-input-set-key (kbd "s-f") 'windmove-right)
                ; resize
                (exwm-input-set-key (kbd "s-C-b") 'my/windresize-left)
                (exwm-input-set-key (kbd "s-C-n") 'my/windresize-down)
                (exwm-input-set-key (kbd "s-C-p") 'my/windresize-up)
                (exwm-input-set-key (kbd "s-C-f") 'my/windresize-right))))
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
  :commands ace-window
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

  (with-eval-after-load "init-exwm"
    (add-hook 'my/exwm-config-hook
              (lambda ()
                ; advanced
                (exwm-input-set-key (kbd "s-l") 'ace-window))))
  :config
  ;(ace-window-display-mode t)

  (defun my/window-split-vert (window)
    (aw-split-window-vert window)
    (windmove-down))

  (defun my/window-split-horz (window)
    (aw-split-window-horz window)
    (windmove-right)))

(use-package hydra
  :ensure t
  :bind (("s-q" . hydra-window-layout/body))
  :config
  (defhydra hydra-window-layout ()
    "layout"
    ("a" ace-window "ace" :color blue)
    ("P" my/windresize-up)
    ("p" windmove-up "up")
    ("F" my/windresize-right)
    ("f" windmove-right "right")
    ("N" my/windresize-down)
    ("n" windmove-down "down")
    ("B" my/windresize-left)
    ("b" windmove-left "left")
    ("u" winner-undo "undo")
    ("U" winner-redo "redo")
    ("SPC" (when (fboundp 'my/hydra-modes-pop)
             (my/hydra-modes-pop)) "cancel" :color blue)))

(provide 'init-window)

;;; init-window.el ends here
