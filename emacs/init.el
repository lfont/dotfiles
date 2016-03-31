;;; init.el --- user settings
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

;; list the repositories containing packages
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; load user's settings
(use-package load-dir
  :ensure t
  :init
  (setq load-dirs t))

;;; init.el ends here
