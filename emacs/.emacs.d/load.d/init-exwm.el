;; -*- lexical-binding: t -*-
;;; init-exwm.el --- window manager
;;; Commentary:
;;; Code:

(defvar my/exwm-config-hook nil
  "Hook called when package's config is executed.")

(use-package exwm
  ;:load-path "~/.emacs.d/site-lisp/exwm/"
  :ensure t
  :commands exwm-enable
  :bind (("C-c b"   . exwm-workspace-switch-to-buffer)
         ("C-c s"   . exwm-workspace-swap)
         ("C-c C-m" . exwm-workspace-move-window)
         :map exwm-mode-map
         ("C-c b" . exwm-workspace-switch-to-buffer)
         ("C-c s" . exwm-workspace-swap)
         ("C-q"   . exwm-input-send-next-key))
  :init
  ;; (setq debug-on-error t
  ;;       debug-on-quit t
  ;;       edebug-all-forms t
  ;;       exwm-debug-on t)

  (setq exwm-workspace-number 6)
  :config
  ;(use-package xelb :load-path "~/.emacs.d/site-lisp/xelb/")
  (use-package s    :ensure t)

  ;; Rename buffer based on its title / class
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (if (s-contains? exwm-class-name exwm-title t)
                  (exwm-workspace-rename-buffer exwm-title)
                (exwm-workspace-rename-buffer (concat exwm-title " - " exwm-class-name)))))

  ;; Local key binding prefix for line mode
  (push ?\C-q exwm-input-prefix-keys)

  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             (when (and exwm-class-name
  ;;                        (string= exwm-class-name "URxvt"))
  ;;               (setq-local exwm-input-prefix-keys '()))))

  ;; 's-q': Reset
  (exwm-input-set-key (kbd "s-q") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda () (interactive) (exwm-workspace-switch ,i))))

  ;; 's-o': Launch application
  (exwm-input-set-key (kbd "s-o")
                      (lambda (command)
                        (interactive (list (read-shell-command "Run: ")))
                        (start-process-shell-command command nil command)))

  ;; 's-<tab>: Switch buffer
  (defun my/exwm-buffer-switch ()
    (interactive)
    (let* ((current-buffers (cl-remove-if-not (lambda (b)
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
  (my/exwm-global-command "s-M-t" "urxvt -e bash -c 'tmux attach -t term || tmux new -s term'")
  (my/exwm-global-command "s-M-l" "slock")
  (my/exwm-global-command "s-M-f" "pcmanfm")
  (my/exwm-global-command "s-M-w" (getenv "BROWSER"))
  (my/exwm-global-command "s-M-e" (getenv "VISUAL"))

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
     ([?\C-k] . (S-end delete))
     ([?\M-w] . (?\C-c))
     ([?\C-w] . (?\C-x))
     ([?\C-y] . (?\C-v))))

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
                                         "pcmanfm --daemon-mode"
                                         "syncthing -no-browser -logflags=3"
                                         "btsync --config ~/.config/btsync/sync.conf"
                                         ; systray
                                         "xfce4-power-manager"
                                         "nm-applet"
                                         "clipit"
                                         "blueman-applet"
                                         "pasystray"))) t)

  ;; Multi monitor support
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-2" 1 "HDMI-2" 2 "HDMI-2"
                                            3 "eDP-1" 4 "eDP-1" 5 "eDP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --primary --output HDMI-2 --left-of eDP-1 --auto")))
  (exwm-randr-enable)

  ;; Date & time
  (use-package minibuffer-line
    :ensure t
    :init
    (setq minibuffer-line-refresh-interval 20
          minibuffer-line-format '((:eval
                                    (format-time-string "%a %Y/%m/%d %I:%M %p"))))
    :config
    (custom-set-faces
     '(minibuffer-line ((t (:height 1.0
                            :weight regular
                            :foreground "LightBlue3")))))
    (minibuffer-line-mode)))

(provide 'init-exwm)

;;; init-exwm.el ends here
