;;; init-ediff.el --- Saner ediff default
;;; Commentary:
;;; Code:

(use-package ediff
  :defer t
  :init
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)

  (defun command-line-diff (switch)
    "Add a SWITCH to invoke Emacs in diff mode."
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left)))
      (ediff file1 file2)))

  (add-to-list 'command-switch-alist '("diff" . command-line-diff))

  (defun command-line-merge (switch)
    "Add a SWITCH to invoke Emacs in merge mode."
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left))
          (file3 (pop command-line-args-left))
          (file4 (pop command-line-args-left)))
      (ediff-merge-files-with-ancestor file1 file2 file3 nil file4)))

  (add-to-list 'command-switch-alist '("merge" . command-line-merge)))

(provide 'init-ediff)

;;; init-ediff.el ends here
