;;; init-prodigy.el --- tasks manager
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure t
  :bind (("C-c t" . prodigy))
  :config
  (use-package nvm   :ensure t)

  (prodigy-define-tag
    :name 'default
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "boost-offerdb - api"
    :cwd "~/code/vp/boost/offer-db/OfferDB.Api"
    :command "dotnet"
    :args '("run")
    :url "http://localhost:46982/swagger/ui"
    :env '(("ASPNETCORE_ENVIRONMENT" "DEV")
           ("ASPNETCORE_URLS" "http://localhost:46982"))
    :tags '(default))

  (prodigy-define-service
    :name "boost-back - api"
    :cwd "~/code/vp/boost/boost-back/Boost.Api"
    :command "dotnet"
    :args '("run")
    :url "http://localhost:56942/swagger/ui"
    :env '(("ASPNETCORE_ENVIRONMENT" "DEV")
           ("ASPNETCORE_URLS" "http://localhost:56942"))
    :tags '(default))

  (prodigy-define-service
    :name "boost-front - front"
    :cwd "~/code/vp/boost/boost-front/Boost"
    :command "dotnet"
    :args '("run")
    :port 60759
    :env '(("ASPNETCORE_ENVIRONMENT" "Development")
           ("ASPNETCORE_URLS" "http://localhost:60759"))
    :tags '(default))

  (prodigy-define-service
    :name "boost-front - webpack"
    :cwd "~/code/vp/boost/boost-front/Boost/wwwroot"
    :command "npm"
    :args '("run" "watch")
    :init-async (lambda (done)
                  (nvm-use "6.10.2" done))
    :tags '(default))

  ;; Nix Shell
  (prodigy-define-tag
    :name 'nix-shell
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args (prodigy-callback (service)
            `("--pure"
              "--run"
              ,(getf service :arg)))
    :stop-signal 'kill)

  ;; (prodigy-define-service
  ;;   :name ""
  ;;   :cwd "~/code/"
  ;;   :arg ""
  ;;   :port 8080
  ;;   :tags '(default nix-shell))

  ;; SSH Tunnel: https://raw.githubusercontent.com/rejeep/prodigy.el/master/examples/ssh-tunnel.el
  (defun my/prodigy-build-tunnel-args (args)
    "Assemble the ssh tunnel argument list."
    `("-v" ;; allows us to parse for the ready message
      "-N" ;; don't start an interactive shell remotely
      "-C" ;; enable compression
      "-L" ,(concat (cl-getf args :localport) ;; the tunnel spec
                    ":"
                    (cl-getf args :tunnel-host)
                    ":"
                    (cl-getf args :tunnel-port))
      "-l" ,(cl-getf args :user) ;; the user name
      "-p" ,(cl-getf args :port) ;; the remote port
      ,(cl-getf args :host)))    ;; the remote host

  (prodigy-define-tag
    :name 'ssh-tunnel
    :command "ssh"
    :cwd (getenv "HOME")
    :args (prodigy-callback (service)
            (my/prodigy-build-tunnel-args
             (cl-getf service :tunnel)))
    :ready-message "debug1: Entering interactive session.")

  ;; (prodigy-define-service
  ;;   :name "tunnel - 10001::81"
  ;;   :tags '(default ssh-tunnel)
  ;;   :tunnel (list
  ;;            :localport   "10001"
  ;;            :tunnel-host ""
  ;;            :tunnel-port "81"
  ;;            :user        ""
  ;;            :host        ""
  ;;            :port        "22"))

  (defun my/prodigy-build-proxy-args (args)
    "Assemble the ssh proxy argument list."
    `("-v" ;; allows us to parse for the ready message
      "-N" ;; don't start an interactive shell remotely
      "-C" ;; enable compression
      "-D" ,(cl-getf args :localport) ;; the proxy spec
      "-l" ,(cl-getf args :user) ;; the user name
      "-p" ,(cl-getf args :port) ;; the remote port
      ,(cl-getf args :host))) ;; the remote host

  (prodigy-define-tag
    :name 'ssh-proxy
    :command "ssh"
    :cwd (getenv "HOME")
    :args (prodigy-callback (service)
            (my/prodigy-build-proxy-args
             (cl-getf service :proxy)))
    :ready-message "debug1: Entering interactive session.")

  (prodigy-define-service
    :name "proxy - 8888:bibimbap.me"
    :tags '(default ssh-proxy)
    :proxy (list
            :localport "8888"
            :user      (getenv "USER")
            :host      "bibimbap.me"
            :port      "22")))

(provide 'init-prodigy)

;;; init-prodigy.el ends here
