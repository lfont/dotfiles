;;; init-prodigy.el --- tasks manager
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure t
  :bind (("C-c t" . prodigy))
  :config
  (use-package nvm   :ensure t)
  (use-package rbenv :ensure t)

  (prodigy-define-service
    :name "Engine"
    :cwd "~/code/Fasterize/FasterizeEngine"
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args '("--pure" "--run" "supervisor devfe")
    :port 8080
    :tags '(work fasterize devfe)
    :stop-signal 'kill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Engine - .devfe.fasterized.net"
    :cwd "~/code/Fasterize/FasterizeEngine"
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args '("--pure" "--run" "supervisor -- devfe --origin_port 80 --secure_origin_port 443")
    :port 8080
    :tags '(work fasterize devfe)
    :stop-signal 'kill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Testerize"
    :cwd "~/code/Fasterize/testerize"
    :path "~/.nvm/versions/node/v0.12.7/bin"
    :command "supervisor"
    :args '("app.js")
    :port 3001
    :tags '(work fasterize devfe)
    :kill-process-buffer-on-stop t
    :init-async (lambda (done)
                  (nvm-use "0.12.7" done)))

  (prodigy-define-service
    :name "Geonosis"
    :cwd "~/code/Fasterize/geonosis"
    :path "~/code/Fasterize/geonosis"
    :command "sbt"
    :args '("run" "-Dconfig.file=../FasterizeEngine/geonosis.conf")
    :port 9000
    :tags '(work fasterize devfe)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "FastAPI"
    :cwd "~/code/Fasterize/fastapi"
    :path "~/.nvm/v0.8.261/bin"
    :command "supervisor"
    :args '("app.js")
    :port 8101
    :tags '(work fasterize devfe)
    :kill-process-buffer-on-stop t
    :init-async (lambda (done)
                  (nvm-use "0.8.261" done)))

  (prodigy-define-service
    :name "fasterize.com"
    :cwd "~/code/Fasterize/fasterize.com"
    :path "~/.rbenv/shims"
    :command "bundle"
    :args '("exec" "rails" "s")
    :port 3000
    :tags '(work fasterize devfe)
    :kill-process-buffer-on-stop t
    :init (lambda ()
            (rbenv-use "1.9.3-p392")
            (setenv "RBENV_VERSION"))))

(provide 'init-prodigy)

;;; init-prodigy.el ends here
