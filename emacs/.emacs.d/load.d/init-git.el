;;; init-git.el --- git setup
;;; Commentary:
;;; Code:

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :defer t
  :init
  (add-hook 'text-mode-hook 'git-gutter-mode)
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'css-mode-hook  'git-gutter-mode)
  :config
  (custom-set-variables '(git-gutter:separator-sign "|"))
  (set-face-foreground 'git-gutter:separator "grey"))

(use-package magit
  :ensure t
  :bind (("C-c g" . hydra-git/body))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (use-package hydra :ensure t)

  (defhydra hydra-git (:color red)
    "git"
    ("j" git-gutter:next-hunk "next hunk")
    ("k" git-gutter:previous-hunk "prev hunk")
    ("t" git-gutter:toggle "toggle")
    ("s" magit-status "status")
    ("l" magit-log-buffer-file "file log")
    ("b" magit-blame "blame")
    ("q" nil "cancel" :color blue)))

(provide 'init-git)

;;; init-git.el ends here
