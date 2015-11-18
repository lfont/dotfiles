(require 'hydra)

(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("b" bookmark-set "bookmark")
  ("j" bookmark-jump "go bookmark")
  ("q" nil "cancel"))

(defun my/line ()
  (interactive)
  (hydra-goto-line/body))

(provide 'my/line)
