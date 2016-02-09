;; -*- lexical-binding: t -*-
;;; hook-run-once.el --- Run a hook only once
;;; Commentary:
;;; Code:

(defvar hook-run-once-fn-alist nil)

(defun hook-run-once-remove-fn (id)
  (let ((as (assoc id hook-run-once-fn-alist)))
    (setq hook-run-once-fn-alist (delq as hook-run-once-fn-alist))
    (cdr as)))

(defun hook-run-once-add-fn (id fn)
  (add-to-list 'hook-run-once-fn-alist (cons id fn)))

(defun hook-run-once (hook function)
  "For the given HOOK run FUNCTION until it returns t."
  (let* ((fn-id (cl-gensym))
         (fn (lambda ()
               (when (funcall function)
                 (remove-hook hook (hook-run-once-remove-fn fn-id))))))
    (hook-run-once-add-fn fn-id fn)
    (add-hook hook fn)))

(provide 'hook-run-once)

;;; hook-run-once.el ends here
