
;;; the-patch.el --- Patching functions in other packages

(require 'the-package)

(use-package el-patch
  :straight (:host github
           :repo "raxod502/el-patch"
           :branch "develop")
  :config

  ;; When patching variable definitions, override the original values.
  (setq el-patch-use-aggressive-defvar t)

  ;; Support for deferred installation in `el-patch-validate-all'.

  (defun the-require-with-deferred-install (feature &rest args)
    "Require FEATURE, installing PACKAGE if necessary.
\(fn FEATURE &optional PACKAGE)"
    (let ((package feature))
      (when args
        (setq package (car args)))
      (when package
        (use-package-install-deferred-package package :el-patch))
      (require feature)))

  (setq el-patch-require-function #'the-require-with-deferred-install))

(provide 'the-patch)

;;; the-patch.el ends here
