;;; init-ivy.el --- Ivy settings
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :ensure ivy-hydra
  :ensure swiper
  :ensure counsel
  :ensure smex
  :diminish ivy-mode
  :demand t
  :bind (("C-s"     . swiper)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x c o" . counsel-imenu)
         ("C-x c s" . counsel-ag)
         ("C-x c g" . counsel-git-grep)
         ("C-x c b" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 12

        magit-completing-read-function 'ivy-completing-read
        mu4e-completing-read-function 'ivy-completing-read
        projectile-completion-system 'ivy)
  :config
  (ido-mode -1)
  (ivy-mode 1))

(provide 'init-ivy)

;;; init-ivy.el ends here
