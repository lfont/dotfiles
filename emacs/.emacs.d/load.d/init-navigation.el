;;; init-navigation.el --- Line settings
;;; Commentary:
;;; Code:

;; Leader key
;; http://emacs.stackexchange.com/questions/12961/how-can-i-globally-replace-c-c-with-another-key-binding
(define-key input-decode-map (kbd "M-<SPC>") (kbd "<leader>"))

;; Global bindings
(global-set-key (kbd "C-c e j") 'join-line)

(use-package linum
  :commands my/line-number-toggle
  :bind (("C-c e l" . my/line-number-toggle))
  :config
  (defun my/vcs-gutter-mode (state)
    (when (fboundp 'git-gutter-mode)
      (git-gutter-mode state)))

  (defun my/line-number-toggle ()
    (interactive)
    (if (call-interactively 'linum-mode)
        (my/vcs-gutter-mode -1)
      (my/vcs-gutter-mode t))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c c" . hydra-cursor/body))
  :commands hydra-cursor/body
  :config
  (use-package hydra :ensure t)

  (defhydra hydra-cursor ()
    "cursor"
    ("n" mc/mark-next-like-this "next")
    ("p" mc/mark-previous-like-this "prev")
    ("N" mc/unmark-next-like-this "unmark next")
    ("P" mc/unmark-previous-like-this "unmark prev")
    ("sn" mc/skip-to-next-like-this "skip next")
    ("sp" mc/skip-to-previous-like-this "skip prev")
    ("e" mc/edit-lines "edit lines")
    ("q" (progn (mc/keyboard-quit)
                (mc/keyboard-quit)
                (when (fboundp 'my/hydra-modes-pop)
                  (my/hydra-modes-pop))) "cancel" :color blue)))

(use-package hydra
  :ensure t
  :commands hydra-modes/body
  :bind (("C-c m" . hydra-modes/body))
  :init
  (defvar my/hydra-modes-stack nil)

  (defun my/hydra-modes-push (expr)
    (push `(lambda () ,expr) my/hydra-modes-stack))

  (defun my/hydra-modes-pop ()
    (interactive)
    (let ((x (pop my/hydra-modes-stack)))
      (when x
        (funcall x))))
  :config
  (defhydra hydra-modes (:color teal)
    "modes"
    ("c" (progn
           (hydra-cursor/body)
           (my/hydra-modes-push '(hydra-modes/body))) "cursor")
    ("g" (progn
           (hydra-git/body)
           (my/hydra-modes-push '(hydra-modes/body))) "git")
    ("l" (progn
           (hydra-window-layout/body)
           (my/hydra-modes-push '(hydra-modes/body))) "layout")
    ("s" (progn
           (hydra-spellcheck/body)
           (my/hydra-modes-push '(hydra-modes/body))) "spellcheck")
    ("q" nil "cancel")))

(use-package modalka
  :ensure t
  :bind (("<leader>" . modalka-mode))
  :init
  (setq-default cursor-type '(bar . 1))
  (setq modalka-cursor-type 'box)

  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)
  :config
  (modalka-define-kbd "%" "M-%")
  (modalka-define-kbd "<" "M-<")
  (modalka-define-kbd ">" "M->")
  (modalka-define-kbd "/" "C-/")
  (modalka-define-kbd "SPC" "C-c m")

  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")

  (modalka-define-kbd "B" "M-b")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "F" "M-f")
  (modalka-define-kbd "G" "M-g g")
  (modalka-define-kbd "L" "C-l")
  (modalka-define-kbd "W" "M-w")
  (modalka-define-kbd "Y" "M-y")

  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "j" "C-c e j")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "l" "C-c e l")
  (modalka-define-kbd "m" "C-SPC")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "s" "C-x c M-s o")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "y" "C-y")

  (modalka-define-kbd "cb" "C-c p b")
  (modalka-define-kbd "cd" "C-c p d")
  (modalka-define-kbd "cf" "C-c p f")
  (modalka-define-kbd "ck" "C-c p k")
  (modalka-define-kbd "cp" "C-c p p")
  (modalka-define-kbd "rm" "C-x r m")
  (modalka-define-kbd "rb" "C-x r b")
  (modalka-define-kbd "xb" "C-x b")
  (modalka-define-kbd "xc" "C-x C-c")
  (modalka-define-kbd "xd" "C-x d")
  (modalka-define-kbd "xe" "C-x C-e")
  (modalka-define-kbd "xf" "C-x C-f")
  (modalka-define-kbd "xk" "C-x k")
  (modalka-define-kbd "xo" "C-x o")
  (modalka-define-kbd "xs" "C-x C-s"))

(provide 'init-navigation)

;;; init-navigation.el ends here
