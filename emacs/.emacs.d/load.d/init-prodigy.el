;;; init-prodigy.el --- tasks manager
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure t
  :bind (("C-c t" . prodigy))
  :config
  (use-package nvm   :ensure t)
  (use-package rbenv :ensure t)

  (prodigy-define-tag
    :name 'fasterize
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "bstats-server"
    :cwd "~/code/Fasterize/bstats-server"
    :path "~/.nvm/v0.10.29/bin"
    :command "supervisor"
    :args '("app.js")
    :port 3100
    :tags '(fasterize)
    :init-async (lambda (done)
                  (nvm-use "0.10.29" done)))

  (prodigy-define-service
    :name "fasterize.com"
    :cwd "~/code/Fasterize/fasterize.com"
    :path "~/.rbenv/shims"
    :command "bundle"
    :args '("exec" "rails" "s")
    :port 3000
    :tags '(fasterize)
    :init (lambda ()
            (rbenv-use "1.9.3-p392")
            (setenv "RBENV_VERSION")))

  (prodigy-define-service
    :name "geonosis"
    :cwd "~/code/Fasterize/geonosis"
    :path "~/code/Fasterize/geonosis"
    :command "sbt"
    :args '("run" "-Dconfig.file=../FasterizeEngine/geonosis.conf")
    :port 9000
    :tags '(fasterize))

  (prodigy-define-service
    :name "reverse-devfe"
    :cwd "~/code/Fasterize/fstrz"
    :path "~/code/Fasterize/fstrz/bin"
    :command "fstrz"
    :args '("container" "reverse-devfe")
    :tags '(fasterize))

  (prodigy-define-service
    :name "testerize"
    :cwd "~/code/Fasterize/testerize"
    :path "~/.nvm/versions/node/v0.12.7/bin"
    :command "supervisor"
    :args '("app.js")
    :port 3001
    :tags '(fasterize)
    :init-async (lambda (done)
                  (nvm-use "0.12.7" done)))

  ;; Nix Shell
  (prodigy-define-tag
    :name 'nix-shell
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args (prodigy-callback (service)
            `("--pure" "--run" ,(getf service :arg)))
    :stop-signal 'kill)

  (prodigy-define-service
    :name "FasterizeEngine"
    :cwd "~/code/Fasterize/FasterizeEngine"
    :arg "supervisor devfe"
    :port 8080
    :tags '(fasterize nix-shell))

  (prodigy-define-service
    :name "FasterizeEngine - .devfe.fasterized.net"
    :cwd "~/code/Fasterize/FasterizeEngine"
    :arg "supervisor -- devfe --origin_port 80 --secure_origin_port 443"
    :port 8080
    :tags '(fasterize nix-shell))

  (prodigy-define-service
    :name "fastapi"
    :cwd "~/code/Fasterize/fastapi"
    :arg "supervisor app.js"
    :port 8101
    :tags '(fasterize nix-shell))

  ;; SSH Tunnel: https://raw.githubusercontent.com/rejeep/prodigy.el/master/examples/ssh-tunnel.el
  (defun my/prodigy-build-tunnel-args (args)
    "Assemble the ssh tunnel argument list."
    `("-v" ;; allows us to parse for the ready message
      "-N" ;; don't start an interactive shell remotely
      "-L" ,(concat (getf args :localport) ;; the tunnel spec
                    ":"
                    (getf args :tunnel-host)
                    ":"
                    (getf args :tunnel-port))
      "-l" ,(getf args :user) ;; the user name
      "-p" ,(getf args :port) ;; the remote port
      ,(getf args :host)))    ;; the remote host

  (prodigy-define-tag
    :name 'ssh-tunnel
    :command "ssh"
    :cwd (getenv "HOME")
    :args (prodigy-callback (service)
            (my/prodigy-build-tunnel-args
             (getf service :tunnel)))
    :ready-message "debug1: Entering interactive session.")

  (prodigy-define-service
    :name "HAProxy - 10001:front01-dc1"
    :tags '(fasterize ssh-tunnel)
    :tunnel (list
             :localport  "10001"
             :tunnel-host  "front01-dc1.fstrz.net"
             :tunnel-port  "81"
             :user  "lfont"
             :host  "front01-dc1.fstrz.net"
             :port  "22"))

  (prodigy-define-service
    :name "HAProxy - 10002:front02-dc1"
    :tags '(fasterize ssh-tunnel)
    :tunnel (list
             :localport  "10002"
             :tunnel-host  "front02-dc1.fstrz.net"
             :tunnel-port  "81"
             :user  "lfont"
             :host  "front02-dc1.fstrz.net"
             :port  "22")))

(provide 'init-prodigy)

;;; init-prodigy.el ends here
