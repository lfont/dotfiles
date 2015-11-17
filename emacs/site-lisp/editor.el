;; Backups
(setq make-backup-files nil ; stop creating those backup~ files
      auto-save-default nil) ; stop creating those #autosave# files

;; Remove trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Put a new line at the end of file
(setq require-final-newline t)

;; ({[ Pairing
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Scroll line by line
(setq scroll-step           1
      scroll-conservatively 10000)

;; Line wrapping
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Use xclip to copy/paste to the terminal from X.
(require 'xclip)
(xclip-mode t)
;(turn-on-xclip) ;; this function is not call in rxvt

;; Always ALWAYS use UTF-8
(require 'iso-transl)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Remote file access
(require 'tramp)
(setq tramp-default-method "sshx")
(add-to-list 'tramp-default-proxies-alist
             '("\\`bibimbap\\'" "\\`root\\'" "/ssh:%h:"))
