;;; init-code.el --- coding tools setup
;;; Commentary:
;;; Code:

;; Syntax checking
(use-package flycheck
  :ensure t
  :diminish "fc"
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; ({[ Pairing
(use-package elec-pair
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Folding
(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Project management
(use-package projectile
  :ensure t
  :defer t
  :init
  (setq projectile-mode-line '(:eval
                               (if (file-remote-p default-directory)
                                   " pt"
                                 (format " pt[%s]" (projectile-project-name)))))

  (add-hook 'conf-mode-hook 'projectile-mode)
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode))

;; Languages modes
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\'"   . web-mode)
         ("\\.tsx\\'"   . web-mode))
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (string= major-mode "web-mode")
                  (turn-off-fci-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my/tide-setup)))))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
              ("C-c C-i" . json-mode-beautify)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local
                              flycheck-javascript-eslint-executable
                              (flycheck-locate-config-file-ancestor-directories
                               "node_modules/.bin/eslint" nil)))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook 'my/tide-setup))

(use-package tide
  :ensure t
  :diminish tide-mode
  :commands my/tide-setup
  :bind (:map tide-mode-map
              ("C-c C-i" . my/tide-format))
  :config
  (defun my/tide-setup ()
    (tide-setup)
    (tide-hl-identifier-mode +1))

  (defun my/tide-format ()
    (interactive)
    (if (string-equal "tsx" (file-name-extension buffer-file-name))
        (web-mode-buffer-indent)
      (tide-format))))

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-doc-mode)
                                 (turn-on-haskell-indent))))

(use-package purescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'purescript-mode-hook (lambda ()
   (psc-ide-mode)
   (turn-on-purescript-indentation))))

(use-package psc-ide
  :ensure t
  :defer t
  :init
  (setq psc-ide-use-npm-bin t))

(use-package nix-mode
  :ensure t
  :defer t)

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda ()
                                (omnisharp-mode))))

(use-package omnisharp
  :ensure t
  :diminish "o#"
  :bind (:map omnisharp-mode-map
              ("<f12>" . omnisharp-go-to-definition)
              ("M-." . omnisharp-go-to-definition)
              ("C-<f12>" . omnisharp-go-to-implementation))
  :init
  (setq omnisharp-server-executable-path "/opt/omnisharp-roslyn/OmniSharp.exe"))

(provide 'init-code)

;;; init-code.el ends here
