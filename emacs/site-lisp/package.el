;; Packages settings
(require 'package)

(let ((package-list '(helm
                      projectile
                      helm-projectile

                      magit
                      git-gutter

                      auto-complete

                      flycheck

                      web-mode
                      js2-mode
                      less-css-mode
                      haskell-mode

                      molokai-theme
                      spaceline

                      fill-column-indicator
                      multiple-cursors
                      rainbow-delimiters

                      xclip

                      multi-term

                      hydra
                      ace-window

                      prodigy
                      nvm
                      rbenv

                      jabber)))

  ;; list the repositories containing them
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))
