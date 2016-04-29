;; -*- lexical-binding: t -*-
;;; init-exwm.el --- window manager
;;; Commentary:
;;; Code:

(defvar my/exwm-config-hook nil
  "Hook called when package's config is executed")

(use-package exwm
  :load-path "~/.emacs.d/site-lisp/exwm/"
  :commands exwm-enable
  :bind (("C-c b" . exwm-workspace-switch-to-buffer)
         :map exwm-mode-map
         ("C-c b" . exwm-workspace-switch-to-buffer)
         ("C-q"   . exwm-input-send-next-key))
  :init
  ;; (setq debug-on-error t
  ;;       debug-on-quit t
  ;;       edebug-all-forms t
  ;;       exwm-debug-on t)

  (setq exwm-workspace-number 6)
  :config
  (use-package xelb :load-path "~/.emacs.d/site-lisp/xelb/")

  ;; Rename buffer based on its class / title
  ;; (add-hook 'exwm-update-class-hook
  ;;           (lambda ()
  ;;             (exwm-workspace-rename-buffer (concat exwm-title " (" exwm-class-name ")"))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer (concat exwm-title " - " exwm-class-name))))

  ;; Local key binding prefix for line mode
  (push ?\C-q exwm-input-prefix-keys)

  ;; 's-q': Reset
  (exwm-input-set-key (kbd "s-q") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda () (interactive) (exwm-workspace-switch ,i))))

  ;; 's-p': Launch application
  (exwm-input-set-key (kbd "s-o")
                      (lambda (command)
                        (interactive (list (read-shell-command "Run: ")))
                        (start-process-shell-command command nil command)))

  ;; 's-<tab>: Switch buffer
  (defun my/exwm-buffer-switch ()
    (interactive)
    (let* ((current-buffers (remove-if-not (lambda (b)
                                             (with-current-buffer b
                                               (eq exwm--frame exwm-workspace--current)))
                                           (mapcar (lambda (pair)
                                                     (cdr pair))
                                                   exwm--id-buffer-alist)))
           (current-buffer-position (cl-position (current-buffer) current-buffers))
           (next-buffer (if (or (not current-buffer-position)
                                (= current-buffer-position (- (length current-buffers) 1)))
                            (nth 0 current-buffers)
                          (nth (+ current-buffer-position 1) current-buffers))))
      (switch-to-buffer next-buffer nil t)))

  (exwm-input-set-key (kbd "s-<tab>") #'my/exwm-buffer-switch)

  ;; Global apps shortcuts
  (defun my/exwm-global-command (key command)
    (exwm-input-set-key (kbd key) (lambda ()
                                    (interactive)
                                    (start-process-shell-command command nil command))))

  (my/exwm-global-command "<XF86AudioRaiseVolume>" "audio-volume.sh up")
  (my/exwm-global-command "<XF86AudioLowerVolume>" "audio-volume.sh down")
  (my/exwm-global-command "s-C-t" "urxvt")
  (my/exwm-global-command "s-C-s" "slock")
  (my/exwm-global-command "s-C-f" "pcmanfm")
  (my/exwm-global-command "s-C-w" (getenv "BROWSER"))
  (my/exwm-global-command "s-C-e" (getenv "VISUAL"))
  ;(my/exwm-global-command "s-C-m" "mail.sh")
  ;(my/exwm-global-command "s-C-c" "jabber.sh")

  ;; Hook for load.d modules
  (run-hooks 'my/exwm-config-hook)

  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))

  ;; System tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Services
  (defun my/exwm-start-processes (processes)
    (let ((delay 0))
      (dolist (process processes)
        (let ((program (car (split-string process))))
          (run-at-time (format "%d sec" delay) nil
                       (lambda ()
                         (start-process-shell-command process nil process)
                         (add-hook 'kill-emacs-hook
                                   (lambda ()
                                     (start-process (concat "pkill " program)
                                                    nil "pkill" program)) t)))
          (setq delay (+ delay 1))))))

  (add-hook 'exwm-init-hook
            (lambda ()
              (my/exwm-start-processes '(; background services
                                         "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
                                         "xsidle.sh slock"
                                         "xfce4-volumed"
                                         "pcmanfm --daemon-mode"
                                         "syncthing -no-browser -logflags=3"
                                         "btsync --config ~/.config/btsync/sync.conf"
                                         ; systray
                                         "xfce4-power-manager"
                                         "nm-applet"
                                         "clipit"
                                         "skype"
                                         "blueman-applet"
                                         "pasystray"))) t)

  ;; Multi monitor support
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI2" 1 "HDMI2" 2 "HDMI2" 3 "HDMI2"
                                            4 "eDP1" 5 "eDP1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP1 --primary --output HDMI2 --left-of eDP1 --auto")))
  (exwm-randr-enable)

  ;; Date & time
  (use-package time
    :init
    (setq display-time-default-load-average nil
          display-time-day-and-date nil)
    :config
    (display-time-mode t)))

(provide 'init-exwm)

;;; init-exwm.el ends here
