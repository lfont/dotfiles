;;; init-line.el --- Line settings
;;; Commentary:
;;; Code:

(use-package linum
  :bind (("C-c l" . hydra-goto-line/body))
  :config
  (use-package hydra :ensure t)

  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("b" bookmark-set "bookmark")
    ("j" bookmark-jump "go bookmark")
    ("q" nil "cancel")))

(provide 'init-line)

;;; init-line.el ends here
