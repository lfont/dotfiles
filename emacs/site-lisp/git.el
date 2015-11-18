;(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(defun my-git-gutter-setup ()
  (when (require 'git-gutter nil t)
    (custom-set-variables '(git-gutter:separator-sign "|"))
    (set-face-foreground 'git-gutter:separator "grey")
    (git-gutter-mode)))

(add-hook 'text-mode-hook 'my-git-gutter-setup)
(add-hook 'prog-mode-hook 'my-git-gutter-setup)
(add-hook 'css-mode-hook  'my-git-gutter-setup)
