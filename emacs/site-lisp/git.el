;;; git.el --- git setup
;;; Commentary:
;;; Code:

;(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'git-gutter)
(custom-set-variables '(git-gutter:separator-sign "|"))
(set-face-foreground 'git-gutter:separator "grey")

(add-hook 'text-mode-hook 'git-gutter-mode)
(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'css-mode-hook  'git-gutter-mode)

(git-gutter-mode)

(require 'hydra)
(defhydra hydra-git (:color red)
  "git"
  ("j" git-gutter:next-hunk "next hunk")
  ("k" git-gutter:previous-hunk "prev hunk")
  ("t" git-gutter:toggle "toggle")
  ("s" magit-status "status")
  ("l" magit-log-buffer-file "file log")
  ("b" magit-blame "blame")
  ("q" nil "cancel" :color blue))

(global-set-key (kbd "C-c g") 'hydra-git/body)

;;; git.el ends here
