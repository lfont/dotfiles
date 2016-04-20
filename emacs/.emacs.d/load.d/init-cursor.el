;;; init-cursor.el --- Cursor settings
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :ensure t
  :bind (("C-c c" . hydra-cursor/body))
  :config
  (use-package hydra :ensure t)

  (defhydra hydra-cursor (:color red)
    "cursor"
    ("j" mc/mark-next-like-this "next")
    ("k" mc/mark-previous-like-this "prev")
    ("J" mc/unmark-next-like-this "unmark next")
    ("K" mc/unmark-previous-like-this "unmark prev")
    ("n" mc/skip-to-next-like-this "skip next")
    ("p" mc/skip-to-previous-like-this "skip prev")
    ("e" mc/edit-lines "lines")
    ("q" (progn (mc/keyboard-quit) (mc/keyboard-quit)) "cancel" :color blue)))

(provide 'init-cursor)

;;; init-cursor.el ends here
