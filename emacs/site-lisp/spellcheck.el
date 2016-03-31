;;; init-spellcheck.el --- Spell checking
;;; Commentary:
;;; Code:

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :init
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"
        ispell-dictionary "american")

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (use-package hydra
    :ensure t
    :bind (("C-c s" . hydra-spellcheck/body))
    :config
    (require 'ispell)

    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode nil))))

    (let ((langs (list ispell-dictionary "francais")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))

    (defun my/ispell-cycle-languages ()
      "Cycle thought available dictionaries."
      (interactive)
      (let ((lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)))

    (defun my/flyspell-check-next-highlighted-word ()
      "Custom function to spell check next highlighted word."
      (interactive)
      (flyspell-goto-next-error)
      (ispell-word))

    (defhydra hydra-spellcheck (:body-pre (ispell-word)
                                :color red)
      "spellcheck"
      ("l" my/ispell-cycle-languages "language")
      ("n" my/flyspell-check-next-highlighted-word "next")
      ("p" flyspell-check-previous-highlighted-word "prev")
      ("q" nil "cancel"))))

(provide 'init-spellcheck)

;;; init-spellcheck.el ends here
