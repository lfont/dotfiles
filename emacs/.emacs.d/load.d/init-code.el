;;; init-code.el --- coding tools setup
;;; Commentary:
;;; Code:

(setq tags-revert-without-query t)

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

;; Markups modes
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\'"   . web-mode)
         ("\\.tsx\\'"   . web-mode))
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (string= major-mode "web-mode")
                (turn-off-fci-mode)))))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
              ("C-c C-i" . json-mode-beautify)))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

;; Languages modes
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local
                              flycheck-javascript-eslint-executable
                              (flycheck-locate-config-file-ancestor-directories
                               "node_modules/.bin/eslint" nil)))))

(use-package tern
  :ensure t
  :diminish "tr"
  :init
  (eval-after-load 'js2-mode '(add-hook 'js2-mode-hook 'tern-mode))
  (eval-after-load 'web-mode '(add-hook 'web-mode-hook
                                        '(lambda ()
                                           (when (string-equal "jsx" (file-name-extension buffer-file-name))
                                             (tern-mode)))))
  :config
  (use-package company-tern
    :ensure t
    :defer t
    :init
    (eval-after-load 'company '(add-to-list 'company-backends 'company-tern))))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package tide
  :ensure t
  :diminish "td"
  :commands my/tide-setup
  :bind (:map tide-mode-map
              ("C-c C-i" . my/tide-format))
  :init
  (eval-after-load 'typescript-mode '(add-hook 'typescript-mode-hook 'my/tide-setup))
  (eval-after-load 'web-mode '(add-hook 'web-mode-hook
                                        '(lambda ()
                                           (when (string-equal "tsx" (file-name-extension buffer-file-name))
                                             (my/tide-setup)))))
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-tide))

  (defun my/tide-setup ()
    (tide-setup)
    (tide-hl-identifier-mode +1))

  (defun my/tide-format ()
    (interactive)
    (if (string-equal "tsx" (file-name-extension buffer-file-name))
        (web-mode-buffer-indent)
      (tide-format))))


(use-package intero
  :ensure t
  :defer t)

(use-package hindent
  :ensure t
  :defer t
  :init
  (setq hindent-reformat-buffer-on-save t))

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-doc-mode)
                                 (turn-on-haskell-indent)
                                 (intero-mode)
                                 (hindent-mode))))

(use-package purescript-mode
  :ensure t
  :defer t)

(use-package psc-ide
  :ensure t
  :defer t
  :init
  (setq psc-ide-use-npm-bin t)
  (eval-after-load 'purescript-mode '(add-hook 'purescript-mode-hook (lambda ()
                                                                       (psc-ide-mode)
                                                                       (turn-on-purescript-indentation)))))

(use-package elm-mode
  :ensure t
  :defer t
  :init
  (setq elm-format-on-save t
        elm-sort-imports-on-save t
        elm-tags-on-save t
        elm-tags-exclude-elm-stuff nil)
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-elm))
  (use-package flycheck-elm
    :ensure t))

(use-package nix-mode
  :ensure t
  :defer t)

(use-package csharp-mode
  :ensure t
  :defer t)

(use-package omnisharp
  :ensure t
  :diminish "o#"
  :bind (:map omnisharp-mode-map
              ("<f12>" . omnisharp-go-to-definition)
              ("M-." . omnisharp-go-to-definition)
              ("C-<f12>" . omnisharp-go-to-implementation)
              ("C-c C-c" . omnisharp-current-type-information)
              ("C-c C-d" . omnisharp-current-type-documentation))
  :init
  (setq omnisharp-server-executable-path (expand-file-name "~/code/omnisharp-roslyn/artifacts/publish/OmniSharp/default/netcoreapp1.1/OmniSharp"))
  (eval-after-load 'csharp-mode '(add-hook 'csharp-mode-hook 'omnisharp-mode))
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

(use-package fsharp-mode
  :ensure t
  :defer t)

(provide 'init-code)

;;; init-code.el ends here
