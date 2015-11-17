(require 'hydra)

;; Smarter move
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `my-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my-move-beginning-of-line)

;; Navigation operations
(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("b" bookmark-set "bookmark")
  ("j" bookmark-jump "go bookmark")
  ("q" nil "cancel"))

(global-set-key (kbd "C-c g") 'hydra-goto-line/body)

;(require 'multiple-cursors)
(defhydra hydra-cursor (:color red)
  "cursor"
  ("j" mc/mark-next-like-this "next")
  ("k" mc/mark-previous-like-this "prev")
  ("J" mc/unmark-next-like-this "unmark next")
  ("K" mc/unmark-previous-like-this "unmark prev")
  ("n" mc/skip-to-next-like-this "skip next")
  ("p" mc/skip-to-previous-like-this "skip prev")
  ("e" mc/edit-lines "lines")
  ("q" (progn (mc/keyboard-quit) (mc/keyboard-quit)) "cancel" :color blue))

(global-set-key (kbd "C-c c") 'hydra-cursor/body)
