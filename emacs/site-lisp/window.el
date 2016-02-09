;;; Windows related operations

;; Undo & redo
(when (fboundp 'winner-mode)
  (winner-mode t))

(require 'hydra)
;(require 'windmove)
;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs

(defun my/buffer-kill-others ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not
               '(lambda (x) (or (buffer-file-name x)
                                (eq 'dired-mode (buffer-local-value 'major-mode x))))
               (buffer-list))))
  (message "Other buffers killed"))

;; Enlarge/Shrink window
(defun my/hydra-move-splitter-left (delta)
  "Move window splitter DELTA columns left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally delta)
    (enlarge-window-horizontally delta)))

(defun my/hydra-move-splitter-right (delta)
  "Move window splitter DELTA columns right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally delta)
    (shrink-window-horizontally delta)))

(defun my/hydra-move-splitter-up (delta)
  "Move window splitter DELTA columns up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window delta)
    (shrink-window delta)))

(defun my/hydra-move-splitter-down (delta)
  "Move window splitter DELTA columns down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window delta)
    (enlarge-window delta)))

;; Hydra window operations
(defhydra hydra-window (:color red)
  "window"
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("H" my/hydra-move-splitter-left nil)
  ("J" my/hydra-move-splitter-down nil)
  ("K" my/hydra-move-splitter-up nil)
  ("L" my/hydra-move-splitter-right nil)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horz")
  ;("t" transpose-frame "'")
  ("a" ace-window "ace")
  ("s" ace-swap-window "swap")
  ("d" ace-delete-window "del")
  ("i" ace-maximize-window "one-max" :exit t)
  ("o" delete-other-windows "one" :exit t)
  ("b" helm-mini "buf")
  ("B" my/buffer-kill-others "one-buf")
  ;("m" headlong-bookmark-jump "bmk")
  ("q" nil "cancel")
  ("u" winner-undo "undo"))

(defun my/window ()
  (interactive)
  (hydra-window/body))

(provide 'my/window)
