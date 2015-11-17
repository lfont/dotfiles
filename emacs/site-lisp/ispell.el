;; Spell checking
(require 'ispell)

(setq ispell-program-name "aspell"
      ispell-list-command "--list")

(let ((langs '("american" "francais")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun ispell-cycle-languages ()
  "Cycle thought available dictionaries."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode nil))))

(defhydra hydra-spellcheck (:body-pre (ispell-word)
                            :color red)
  "spellcheck"
  ("l" ispell-cycle-languages "language")
  ("n" flyspell-check-next-highlighted-word "next")
  ("p" flyspell-check-previous-highlighted-word "prev")
  ("q" nil "cancel"))

(defun my-ispell ()
  (interactive)
  (hydra-spellcheck/body))

(provide 'my-ispell)
