;; Packages settings
(require 'package)

(let ((package-list '(helm
                      helm-projectile

                      git-gutter
                      magit

                      projectile
                      auto-complete
                      flycheck
                      web-mode
                      js2-mode
                      less-css-mode
                      haskell-mode

                      multiple-cursors

                      molokai-theme
                      spaceline
                      fill-column-indicator
                      rainbow-delimiters

                      xclip

                      multi-term

                      hydra

                      ace-window
                      popwin

                      prodigy
                      nvm
                      rbenv

                      jabber)))

  ;; list the repositories containing them
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))
